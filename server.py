#!/usr/bin/python
from bottle import route, run, request, get, post, put, delete
import sqlite3


#*******************************************************************************
@post('/polls')
def __postPoll():

	return "Hello World!"

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
