from flask import Flask
from flask_sqlalchemy import SQLAlchemy

db = SQLAlchemy()

def create_app():
    app = Flask(__name__)
    app.config['SQLALCHEMY_DATABASE_URI'] = 'postgresql://exam_user:pswrd123@localhost:5432/exam_proctoring'
    app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
    app.secret_key = "supersecretkey"

    db.init_app(app)

    # Import models so they are registered with SQLAlchemy
    from web_app import models

    # Import routes (your big app.py already defines routes)
    import web_app.app  

    return app
