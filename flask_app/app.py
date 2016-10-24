from flask import Flask, render_template
from flask_script import Manager
from flask_bootstrap import Bootstrap
from flask_wtf import Form
from wtforms import SubmitField, IntegerField
from wtforms.validators import NumberRange

app = Flask(__name__)

manager = Manager(app)
bootstrap = Bootstrap(app)


class RatingForm(Form):
	submit = SubmitField('Submit')
	rating = IntegerField('Rate this movie', \
		validators = [NumberRange(0, 5)])


@app.route('/', methods = ['GET', 'POST'])
def index():
    return render_template('index.html')


if __name__ == '__main__':
    manager.run()