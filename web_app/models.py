# web_app/models.py
from flask_sqlalchemy import SQLAlchemy
from sqlalchemy.dialects.postgresql import JSONB
from datetime import datetime
from werkzeug.security import generate_password_hash, check_password_hash
from . import db



db = SQLAlchemy() # don't import app here

class Student(db.Model):
    __tablename__ = "students"
    id = db.Column(db.Integer, primary_key=True)
    student_id = db.Column(db.String(64), unique=True, nullable=False)
    name = db.Column(db.String(200), nullable=True)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)

class Calibration(db.Model):
    __tablename__ = "calibrations"
    id = db.Column(db.Integer, primary_key=True)
    student_id = db.Column(db.String(64), db.ForeignKey("students.student_id"), nullable=False)
    model_type = db.Column(db.String(32), default="isolation_forest")
    meta = db.Column(JSONB)   # stores MCQ answers, environment, browser, device info
    created_at = db.Column(db.DateTime, default=datetime.utcnow)

    student = db.relationship("Student", backref=db.backref("calibrations", lazy="dynamic"))

class CalibrationEvent(db.Model):
    __tablename__ = "calibration_events"
    id = db.Column(db.BigInteger, primary_key=True)
    calibration_id = db.Column(db.Integer, db.ForeignKey("calibrations.id"), nullable=False)
    event_type = db.Column(db.String(32), nullable=False)  # 'keystroke'|'mouse'|'focus'
    event_data = db.Column(JSONB, nullable=False)          # raw event object: {key, type, ts} or {x,y,ts,dt}
    ts = db.Column(db.DateTime, nullable=False, index=True)

    calibration = db.relationship("Calibration", backref=db.backref("events", lazy="dynamic"))

class CalibrationFeature(db.Model):
    __tablename__ = "calibration_features"
    id = db.Column(db.BigInteger, primary_key=True)
    calibration_id = db.Column(db.Integer, db.ForeignKey("calibrations.id"), nullable=False)
    window_start = db.Column(db.DateTime, nullable=False)
    window_end = db.Column(db.DateTime, nullable=False)

    avg_dwell = db.Column(db.Float)
    std_dwell = db.Column(db.Float)
    avg_flight = db.Column(db.Float)
    std_flight = db.Column(db.Float)
    backspace_rate = db.Column(db.Float)
    chars_per_min = db.Column(db.Float)

    mouse_avg_speed = db.Column(db.Float)
    mouse_std_speed = db.Column(db.Float)
    mouse_click_rate = db.Column(db.Float)
    mouse_path_length = db.Column(db.Float)

    feature_vector = db.Column(JSONB)

    calibration = db.relationship("Calibration", backref=db.backref("features", lazy="dynamic"))

class ExamSession(db.Model):
    __tablename__ = "exam_sessions"
    id = db.Column(db.BigInteger, primary_key=True)
    student_id = db.Column(db.String(64), db.ForeignKey("students.student_id"), nullable=False)
    started_at = db.Column(db.DateTime, default=datetime.utcnow)
    ended_at = db.Column(db.DateTime)
    final_anomaly_ratio = db.Column(db.Float)
    flagged = db.Column(db.Boolean, default=False)
    meta = db.Column(JSONB)

    student = db.relationship("Student", backref=db.backref("exam_sessions", lazy="dynamic"))

class ExamEvent(db.Model):
    __tablename__ = "exam_events"
    id = db.Column(db.BigInteger, primary_key=True)
    exam_session_id = db.Column(db.BigInteger, db.ForeignKey("exam_sessions.id"), nullable=False)
    event_type = db.Column(db.String(32), nullable=False)
    event_data = db.Column(JSONB, nullable=False)
    ts = db.Column(db.DateTime, nullable=False, index=True)

    exam_session = db.relationship("ExamSession", backref=db.backref("events", lazy="dynamic"))

class ExamFeature(db.Model):
    __tablename__ = "exam_features"
    id = db.Column(db.BigInteger, primary_key=True)
    exam_session_id = db.Column(db.BigInteger, db.ForeignKey("exam_sessions.id"), nullable=False)
    window_start = db.Column(db.DateTime, nullable=False)
    window_end = db.Column(db.DateTime, nullable=False)

    avg_dwell = db.Column(db.Float)
    std_dwell = db.Column(db.Float)
    avg_flight = db.Column(db.Float)
    std_flight = db.Column(db.Float)
    backspace_rate = db.Column(db.Float)
    chars_per_min = db.Column(db.Float)

    mouse_avg_speed = db.Column(db.Float)
    mouse_std_speed = db.Column(db.Float)
    mouse_click_rate = db.Column(db.Float)
    mouse_path_length = db.Column(db.Float)

    feature_vector = db.Column(JSONB)
    ml_label = db.Column(db.Integer)   # 1 normal, -1 anomaly
    ml_score = db.Column(db.Float)

    exam_session = db.relationship("ExamSession", backref=db.backref("features", lazy="dynamic"))

class ExamFlag(db.Model):
    __tablename__ = "exam_flags"
    id = db.Column(db.BigInteger, primary_key=True)
    exam_session_id = db.Column(db.BigInteger, db.ForeignKey("exam_sessions.id"), nullable=False)
    flag_type = db.Column(db.String(64))
    score = db.Column(db.Float)
    threshold = db.Column(db.Float)
    details = db.Column(JSONB)
    created_at = db.Column(db.DateTime, default=datetime.utcnow)

    exam_session = db.relationship("ExamSession", backref=db.backref("flags", lazy="dynamic"))
class ExamUser(db.Model):
    __tablename__ = 'exam_users'

    id = db.Column(db.Integer, primary_key=True)
    username = db.Column(db.String(50), unique=True, nullable=False)
    email = db.Column(db.String(120), unique=True, nullable=False)
    password = db.Column(db.String(200), nullable=False)
    is_admin = db.Column(db.Boolean, default=False)

    def set_password(self, password):
        self.password_hash = generate_password_hash(password)
    
    def check_password(self, password):
        return check_password_hash(self.password_hash, password)

    def __repr__(self):
        return f"<ExamUser {self.username}>"

class ExamSubmission(db.Model):
    __tablename__ = "exam_submissions"
    id = db.Column(db.Integer, primary_key=True)
    student_id = db.Column(db.String(100), db.ForeignKey("students.student_id"), nullable=False)
    answers = db.Column(db.JSON, nullable=False)  # store answers as JSON
    submitted_at = db.Column(db.DateTime, default=datetime.utcnow)

    student = db.relationship("Student", backref="submissions")

    def __ref__(self):
        return f"<ExamSubmission {self.id} student={self.student_id}>"