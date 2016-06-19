#!/usr/bin/python
from bottle import route, run, request, get, post, put, delete
import sqlite3
import dbio

#*******************************************************************************
@post('/polls')
def __postPoll():
	name: =request.forms.get('name')
    appointments =request.forms.get('appointments)
    password =request.forms.get('password')
    # Überprüfung der Eingaben ...
	return dbio.postPoll(name, appointments, password)

#*******************************************************************************
@get('/polls/<PID>')
def __getPolls(PID=0):
	return "Hello World!"

#*******************************************************************************
@put('/polls<PID>')
def __putPoll():
    return "Hello World!"

#*******************************************************************************
@delete('/polls<PID>')
def __delPoll():
    return "Hello World!"


#a#*******************************************************************************
@post('/polls/<PID>/votes')
def __postVote():
    return "Hello World!"

#*******************************************************************************
@get('/polls/<PID>/votes/<VID>')
def __getVotes():
    return "Hello World!"

#*******************************************************************************
@put('/polls/<PID>/votes/<VID>')
def __putVote():
    return "Hello World!"

#*******************************************************************************
@delete('/polls/<PID>/votes/<VID>')
def __delVote():
    return "Hello World!"



#*******************************************************************************
run(host='localhost', port=8080, debug=True)
