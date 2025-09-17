import os
import time
from threading import Lock
from flask import Flask, render_template, request, jsonify, url_for
from joblib import dump, load
import numpy as np
from datetime import datetime
from web_app.models import db, Student, Calibration, CalibrationEvent, CalibrationFeature, ExamSession, ExamEvent, ExamFeature, ExamFlag, ExamUser, ExamSubmission
from sklearn.ensemble import IsolationForest
from sklearn.svm import OneClassSVM
from werkzeug.security import generate_password_hash, check_password_hash
from flask_socketio import SocketIO, emit
import statistics  # 🔥 added for monitoring stats


# ---------------- App + DB setup ----------------

app = Flask(__name__)

# DB URI
app.config['SQLALCHEMY_DATABASE_URI'] = os.environ.get(
    "DATABASE_URL",
    "postgresql://exam_user:pswrd123@localhost:5432/exam_proctoring"
)
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False

socketio = SocketIO(app, cors_allowed_origins="*")
db.init_app(app)

MODEL_DIR = os.path.join(os.path.dirname(__file__), "..", "models_store")
os.makedirs(MODEL_DIR, exist_ok=True)

baseline_models = {}  # student_id -> sklearn model


# ---------------- Utility ----------------
def extract_keystroke_features_from_raw(raw_key_events):
    presses = []
    releases = []
    for ev in raw_key_events:
        if ev.get("type") == "down":
            presses.append(ev)
        elif ev.get("type") == "up":
            releases.append(ev)

    dwell = []
    for i in range(min(len(presses), len(releases))):
        dwell.append(releases[i]["ts"] - presses[i]["ts"])

    flight = []
    for i in range(1, len(presses)):
        flight.append(presses[i]["ts"] - presses[i - 1]["ts"])

    backspaces = sum(1 for p in presses if p.get("key", "").lower() == "backspace")
    total_chars = sum(1 for p in presses if len(p.get("key", "")) == 1)
    duration_seconds = max(
        1.0, (presses[-1]["ts"] - presses[0]["ts"]) / 1000.0
    ) if presses else 1.0
    chars_per_min = (total_chars / duration_seconds) * 60.0

    if not dwell and not flight:
        return []

    windows = []
    N = 20
    for start in range(0, max(1, len(presses) - N + 1), N):
        end = start + N
        window_d = dwell[start:end] if len(dwell) >= end else dwell[start:len(dwell)]
        window_f = flight[start:end] if len(flight) >= end else flight[start:len(flight)]
        avg_dwell = float(np.mean(window_d)) if window_d else 0.0
        std_dwell = float(np.std(window_d)) if window_d else 0.0
        avg_flight = float(np.mean(window_f)) if window_f else 0.0
        std_flight = float(np.std(window_f)) if window_f else 0.0
        backspace_rate = backspaces / (total_chars + 1e-6)
        windows.append([avg_dwell, std_dwell, avg_flight, std_flight, backspace_rate, chars_per_min])
    return windows


# ---------------- Routes ----------------
@app.route("/")
def home():
    return render_template("base.html")


@app.route("/calibration", methods=["GET"])
def calibration_page():
    return render_template("calibration.html")


@app.route("/api/calibration", methods=["POST"])
def api_calibration():
    payload = request.json
    student_id = payload.get("student_id", "student123")
    answers = payload.get("answers", {})
    model_choice = payload.get("model", "isolation_forest")

    student = Student.query.filter_by(student_id=student_id).first()
    if not student:
        student = Student(student_id=student_id, name=None)
        db.session.add(student)
        db.session.commit()

    cal = Calibration(student_id=student_id, model_type=model_choice, meta={"answers": answers})
    db.session.add(cal)
    db.session.commit()

    raw_events = payload.get("keystrokes") or payload.get("keystroke") or []
    if raw_events:
        for ev in raw_events:
            ts = datetime.utcfromtimestamp(ev.get("ts", 0) / 1000.0) if isinstance(ev.get("ts", 0), (int, float)) else datetime.utcnow()
            ce = CalibrationEvent(calibration_id=cal.id, event_type=ev.get("type", "keystroke"), event_data=ev, ts=ts)
            db.session.add(ce)
        db.session.commit()

    if payload.get("keystrokes") or payload.get("keystroke"):
        raw_keys = payload.get("keystrokes") or payload.get("keystroke")
    else:
        raw_keys = []
        dwell = payload.get("dwell", [])
        ts = int(datetime.utcnow().timestamp() * 1000)
        for i, d in enumerate(dwell):
            raw_keys.append({"type": "down", "key": "k", "ts": ts + i * 50})
            raw_keys.append({"type": "up", "key": "k", "ts": ts + i * 50 + int(d)})

    features = extract_keystroke_features_from_raw(raw_keys)
    if not features:
        return jsonify({"error": "no features produced"}), 400

    X = np.array(features)
    if model_choice == "svm":
        model = OneClassSVM(kernel="rbf", nu=0.1, gamma="scale")
        model.fit(X)
    else:
        model = IsolationForest(contamination=0.1, random_state=42)
        model.fit(X)

    model_path = os.path.join(MODEL_DIR, f"{student_id}.joblib")
    dump(model, model_path)
    baseline_models[student_id] = model

    for win_idx, vec in enumerate(features):
        cf = CalibrationFeature(
            calibration_id=cal.id,
            window_start=datetime.utcnow(),
            window_end=datetime.utcnow(),
            avg_dwell=float(vec[0]),
            std_dwell=float(vec[1]),
            avg_flight=float(vec[2]),
            std_flight=float(vec[3]),
            backspace_rate=float(vec[4]),
            chars_per_min=float(vec[5]),
            feature_vector={"vec": vec}
        )
        db.session.add(cf)
    db.session.commit()

    return jsonify({"message": "calibration saved", "samples": len(features)}), 200


@app.route("/exam", methods=["GET"])
def exam_page():
    return render_template("exam.html")


@app.route("/api/exam_stream", methods=["POST"])
def api_exam_stream():
    payload = request.json
    student_id = payload.get("student_id", "student123")
    session_id = payload.get("session_id")

    exam_session = ExamSession(student_id=student_id, meta={"session_id": session_id})
    db.session.add(exam_session)
    db.session.commit()

    for ev in payload.get("keystrokes", []):
        ts = datetime.utcfromtimestamp(ev.get("ts", 0) / 1000.0) if isinstance(ev.get("ts", 0), (int, float)) else datetime.utcnow()
        db.session.add(ExamEvent(exam_session_id=exam_session.id, event_type="keystroke", event_data=ev, ts=ts))
    for ev in payload.get("mouse", []):
        ts = datetime.utcfromtimestamp(ev.get("ts", 0) / 1000.0) if isinstance(ev.get("ts", 0), (int, float)) else datetime.utcnow()
        db.session.add(ExamEvent(exam_session_id=exam_session.id, event_type="mouse", event_data=ev, ts=ts))
    db.session.commit()

    with thread_lock:
        if session_id not in monitor_threads:
            t = socketio.start_background_task(monitor_exam_session, student_id, session_id)
            monitor_threads[session_id] = t

    return jsonify({"ok": True, "session_db_id": exam_session.id})


@app.route("/thankyou")
def thankyou():
    student_id = request.args.get("student_id", "Unknown")
    submitted_at = request.args.get("submitted_at", "N/A")
    return render_template("thankyou.html", student_id=student_id, submitted_at=submitted_at)


@app.route("/api/exam/submit", methods=["POST"])
def submit_exam():
    data = request.get_json()
    student_id = data.get("student_id")
    answers = data.get("answers")

    if not student_id or not answers:
        return jsonify({"error": "Missing student_id or answers"}), 400

    submission = ExamSubmission(student_id=student_id, answers=answers)
    db.session.add(submission)
    db.session.commit()

    redirect_url = url_for("thankyou", student_id=student_id, submitted_at=submission.submitted_at)
    return jsonify({"status": "success", "redirect": redirect_url}), 200


# ---------------- 🔥 Real-time Monitoring API ----------------
@app.route("/api/exam/monitor", methods=["POST"])
def monitor_activity():
    """Receives keystrokes/mouse events every minute and flags unusual behavior"""
    data = request.get_json()
    student_id = data.get("student_id")
    keystrokes = data.get("keystrokes", [])
    mouse = data.get("mouse", [])

    if not student_id:
        return jsonify({"status": "error", "msg": "No student_id"}), 400

    flags = []

    # Keystroke timing analysis
    if len(keystrokes) > 3:
        intervals = [keystrokes[i + 1]["ts"] - keystrokes[i]["ts"] for i in range(len(keystrokes) - 1)]
        avg_interval = statistics.mean(intervals)
        if avg_interval < 50 or avg_interval > 300:
            flags.append("Irregular typing speed")

    # Mouse activity analysis
    if len(mouse) > 3:
        move_intervals = [mouse[i + 1]["ts"] - mouse[i]["ts"] for i in range(len(mouse) - 1)]
        avg_mouse = statistics.mean(move_intervals)
        if avg_mouse < 30 or avg_mouse > 500:
            flags.append("Unusual mouse activity")

    status = "Normal behavior" if not flags else "⚠️ Flags: " + ", ".join(flags)
    return jsonify({"status": status})


# ---------------- Background Monitoring ----------------
monitor_threads = {}
thread_lock = Lock()

def monitor_exam_session(student_id, session_id):
    with app.app_context():
        while True:
            session = ExamSession.query.filter_by(student_id=student_id).order_by(ExamSession.id.desc()).first()
            if not session:
                break

            events = ExamEvent.query.filter_by(exam_session_id=session.id).all()
            keystrokes = [e.event_data for e in events if e.event_type == "keystroke"]
            mouse = [e.event_data for e in events if e.event_type == "mouse"]

            features = extract_keystroke_features_from_raw(keystrokes)
            flagged = False
            reason = None

            if features:
                model = baseline_models.get(student_id)
                if model:
                    X = np.array(features)
                    preds = model.predict(X)
                    anomaly_count = int((preds == -1).sum())
                    anomaly_ratio = float(anomaly_count) / max(1, len(preds))
                    if anomaly_ratio > 0.2:
                        flagged = True
                        reason = f"Anomaly ratio {round(anomaly_ratio, 2)}"

            if mouse:
                times = [m["ts"] for m in mouse if "ts" in m]
                if times and (max(times) - min(times)) > 20000: #20 seconds interval
                    flagged = True
                    reason = "Mouse inactivity >1min"

            if flagged:
                flag = ExamFlag(
                    exam_session_id=session.id,
                    flag_type="realtime",
                    score=1.0,
                    threshold=0.2,
                    details={"reason": reason}
                )
                db.session.add(flag)
                db.session.commit()

                socketio.emit("exam_flag", {
                    "student_id": student_id,
                    "session_id": session_id,
                    "reason": reason
                })

            db.session.commit()
            time.sleep(60)


# ---------------- Run ----------------
if __name__ == "__main__":
    with app.app_context():
        db.create_all()
        print("Tables created successfully")

        from web_app.models import ExamUser
        if not ExamUser.query.filter_by(username="admin").first():
            admin = ExamUser(username="admin", email="admin@example.com", is_admin=True,
                             password=generate_password_hash("AdminPassword123!"))
            db.session.add(admin)
            db.session.commit()
            print("Admin user created successfully!")

        if not ExamUser.query.filter_by(username="student1").first():
            user = ExamUser(username="student1",
                            password=generate_password_hash("StudentPassword123!"),
                            email="student1@example.com",
                            is_admin=False)
            db.session.add(user)
        db.session.commit()

    socketio.run(app, debug=True)
