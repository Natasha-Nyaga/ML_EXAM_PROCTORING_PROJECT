import json
import time
from datetime import datetime, timezone
import numpy as np
from web_app.models import ExamSession
from flask import Flask, render_template, request, jsonify, Blueprint, session, redirect, url_for
from flask_sqlalchemy import SQLAlchemy
from flask_socketio import SocketIO
from werkzeug.security import generate_password_hash, check_password_hash
from sklearn.ensemble import IsolationForest

# ---------------- Config ----------------
app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = 'postgresql://exam_user:pswrd123@localhost:5432/exam_proctoring'
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
app.secret_key = "supersecretkey"

db = SQLAlchemy(app)
socketio = SocketIO(app, cors_allowed_origins="*")

api = Blueprint("api", __name__)

# ---------------- Models ----------------
class ExamUser(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    username = db.Column(db.String(50), unique=True)
    password_hash = db.Column(db.String(200))
    role = db.Column(db.String(20), default="student")

    def set_password(self, password):
        self.password_hash = generate_password_hash(password)

    def check_password(self, password):
        return check_password_hash(self.password_hash, password)


class ExamSession(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    student_id = db.Column(db.String(50))
    started_at = db.Column(db.DateTime, default=datetime.utcnow)
    ended_at = db.Column(db.DateTime, nullable=True)


class ExamEvent(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    exam_session_id = db.Column(db.Integer, db.ForeignKey("exam_session.id"))
    event_type = db.Column(db.String(50))
    event_data = db.Column(db.JSON)
    ts = db.Column(db.DateTime, default=datetime.utcnow)


class ExamFlag(db.Model):
    id = db.Column(db.Integer, primary_key=True)
    exam_session_id = db.Column(db.Integer, db.ForeignKey("exam_session.id"))
    flag_type = db.Column(db.String(50))
    score = db.Column(db.Float)
    threshold = db.Column(db.Float)
    details = db.Column(db.JSON)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)


with app.app_context():
    db.create_all()
    if not ExamUser.query.filter_by(username="admin").first():
        admin = ExamUser(username="admin", role="admin")
        admin.set_password("AdminPassword123!")
        db.session.add(admin)
        db.session.commit()
    print("Tables created successfully")


# ---------------- ML Utils ----------------
def extract_keystroke_features_from_raw(keystrokes):
    """Simple timing feature extractor"""
    features = []
    last_ts = None
    for k in keystrokes:
        if last_ts is not None:
            dt = k["ts"] - last_ts
            features.append({"vec": [dt]})
        last_ts = k["ts"]
    return features


MODELS = {}
DEMO_ANOMALY_THRESHOLD = 0.3
INACTIVITY_MS = 15000
AVG_MOUSE_SPEED_PX_S = 50
REQUIRED_SIGNALS = 2


def load_or_get_model_for_student(student_id):
    return MODELS.get(student_id)


def compute_mouse_features(mouse_events):
    if not mouse_events:
        return {"inactivity_ms": None, "avg_speed_px_s": None}
    inactivity = 0
    total_speed = 0
    count = 0
    last = None
    for ev in mouse_events:
        if ev["type"] == "move":
            if last:
                dt = ev["ts"] - last["ts"]
                dist = ((ev["x"] - last["x"]) ** 2 + (ev["y"] - last["y"]) ** 2) ** 0.5
                if dt > 0:
                    total_speed += dist / (dt / 1000.0)
                    count += 1
                if dt > inactivity:
                    inactivity = dt
            last = ev
    avg_speed = total_speed / count if count else None
    return {"inactivity_ms": inactivity, "avg_speed_px_s": avg_speed}

def create_exam_session(student_id):
    session = ExamSession(
        student_id=student_id,
        started_at=datetime.now(timezone.utc),
        ended_at=None
    )
    db.session.add(session)
    db.session.commit()
    return session.id
# ---------------- Routes ----------------
@app.route("/")
def index():
    return render_template("calibration.html")


@app.route("/calibration", methods=["GET", "POST"])
def calibration():
    if request.method == "POST":
        try:
            answers = {
                "q1": request.form.get("q1"),
                "q2": request.form.get("q2"),
                "q3": request.form.get("q3"),
                "q4": request.form.get("q4"),
                "q5": request.form.get("q5"),
                "q6": request.form.get("q6"),
                "q7": request.form.get("q7"),
                "q8": request.form.get("q8"),
            }
            mouse_movements = request.form.get("mouse_movements", "[]")
            keystroke_data = request.form.get("keystroke_data", "[]")

            session["calibration_data"] = {
                "answers": answers,
                "mouse_movements": mouse_movements,
                "keystroke_data": keystroke_data,
            }

            return redirect(url_for("exam"))
        except Exception as e:
            return f"Error submitting calibration: {str(e)}"

    return render_template("calibration.html")


@app.route("/exam")
def exam():
    session_id = create_exam_session(student_id="student123")
    return render_template("exam.html", session_id=session_id)
    


@app.route("/thankyou")
def thankyou():
    session_id = request.args.get("session_id")
    session = db.session.get(ExamSession, session_id) if session_id else None
    return render_template("thankyou.html", student_id=session.student_id if session else "Unknown", submission_time=session.ended_at if session else "N/A")


# ---- API Endpoints ----
@api.route("/calibration/submit", methods=["POST"])
def calibration_submit():
    """
    Save calibration keystrokes + mouse baseline and redirect to exam
    """
    data = request.json
    student_id = data.get("student_id")
    keystrokes = data.get("keystrokes", [])
    mouse_data = data.get("mouseEvents", [])  # <-- match frontend key
    answers = data.get("answers", {})

    # Train keystroke model
    features = extract_keystroke_features_from_raw(keystrokes)
    if features:
        X = np.array([f["vec"] for f in features])
        model = IsolationForest(contamination=0.1).fit(X)
        MODELS[student_id] = model

    # Start exam session
    session = ExamSession(student_id=student_id)
    db.session.add(session)
    db.session.commit()

    # Log calibration answers
    db.session.add(ExamEvent(
        exam_session_id=session.id,
        event_type="answers",
        event_data=answers
    ))

    # Log calibration events
    for k in keystrokes:
        db.session.add(ExamEvent(
            exam_session_id=session.id,
            event_type="keystroke",
            event_data=k
        ))
    for m in mouse_data:
        db.session.add(ExamEvent(
            exam_session_id=session.id,
            event_type="mouse",
            event_data=m
        ))
    db.session.commit()

    return jsonify({"status": "calibration saved", "session_id": session.id, "redirect": "/exam"})


@api.route("/event", methods=["POST"])
def log_event():
    """
    Generic endpoint for logging events (mouse, keystroke, visibility)
    """
    data = request.json
    session_id = data.get("session_id")
    event_type = data.get("event_type")
    event_data = data.get("event_data", {})

    if not session_id or not event_type:
        return jsonify({"error": "Missing session_id or event_type"}), 400

    event = ExamEvent(
        exam_session_id=session_id,
        event_type=event_type,
        event_data=event_data
    )
    db.session.add(event)
    db.session.commit()

    return jsonify({"status": "event logged"})


@api.route("/exam/submit", methods=["POST"])
def exam_submit():
    data = request.json
    session_id = data.get("session_id")
    answers = data.get("answers")

    # Save answers to DB
    if session_id:
        session = db.session.get(ExamSession, session_id)
        if session:
            session.ended_at = datetime.now(timezone.utc)
            db.session.commit()

        db.session.add(ExamEvent(
            exam_session_id=session_id,
            event_type="exam_submission",
            event_data=answers
        ))
        db.session.commit()

    return jsonify({"status": "success", "redirect": f"/thankyou?session_id={session_id}"})


# Register blueprint
app.register_blueprint(api, url_prefix="/api")


# ---------------- Background Monitoring ----------------
def background_exam_monitor():
    """Run checks every 20s for suspicious behavior"""
    while True:
        with app.app_context():
            sessions = ExamSession.query.all()
            for session in sessions:
                student_id = session.student_id

                cutoff = datetime.utcnow().timestamp() - 20
                events = ExamEvent.query.filter(
                    ExamEvent.exam_session_id == session.id,
                    ExamEvent.ts >= datetime.utcfromtimestamp(cutoff)
                ).all()

                keystrokes = [e.event_data for e in events if e.event_type == "keystroke"]
                mouse = [e.event_data for e in events if e.event_type == "mouse"]
                visibility = [e.event_data for e in events if e.event_type == "visibility"]

                features = extract_keystroke_features_from_raw(keystrokes)
                model = load_or_get_model_for_student(student_id)

                signals = []

                # Keystroke anomaly
                if model and features:
                    X = np.array([f["vec"] for f in features])
                    preds = model.predict(X)
                    anomaly_ratio = float((preds == -1).sum()) / max(1, len(preds))
                    if anomaly_ratio > DEMO_ANOMALY_THRESHOLD:
                        signals.append(("keystroke", f"ratio {round(anomaly_ratio,3)}"))

                # Mouse features
                mouse_feat = compute_mouse_features(mouse)
                if mouse_feat["inactivity_ms"] and mouse_feat["inactivity_ms"] > INACTIVITY_MS:
                    signals.append(("mouse", f"inactive {mouse_feat['inactivity_ms']//1000}s"))
                elif mouse_feat["avg_speed_px_s"] and mouse_feat["avg_speed_px_s"] < AVG_MOUSE_SPEED_PX_S:
                    signals.append(("mouse", f"slow {round(mouse_feat['avg_speed_px_s'],1)} px/s"))

                # Visibility changes
                for ve in visibility:
                    state = ve.get("state")
                    if state and state.lower() in ("hidden", "prerender", "unfocused"):
                        signals.append(("visibility", f"changed to {state}"))
                        break

                # If enough suspicious signals, raise flag
                if len(signals) >= REQUIRED_SIGNALS:
                    flag = ExamFlag(
                        exam_session_id=session.id,
                        flag_type="background",
                        score=0.0,
                        threshold=DEMO_ANOMALY_THRESHOLD,
                        details={"signals": signals},
                    )
                    db.session.add(flag)
                    db.session.commit()
                    socketio.emit(
                        "exam_flag",
                        {
                            "student_id": student_id,
                            "session_id": session.id,
                            "reason": "Background monitor: " + str(signals),
                            "signals": signals,
                        },
                        broadcast=True,
                    )

        time.sleep(20)


# ---------------- Run ----------------
if __name__ == "__main__":
    socketio.start_background_task(background_exam_monitor)
    socketio.run(app, debug=True)
