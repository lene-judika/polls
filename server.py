#!/usr/bin/python
from bottle import route, run, request, get, post, put, delete
import sqlite3
import dbio
import json

#*******************************************************************************
@post('/polls')
def __postPoll():
	obj = {}
	for key, value in request.POST.iteritems():
		print ("{0}: {1}".format(key, value))
		obj[key]=value
	obj['appointments'] = obj['appointments'].split(',') 
	return dbio.postPoll(obj)
	# Ueberpruefung der Eingaben ...

#*******************************************************************************
@get('/polls/<PID>')
def __getPolls(PID=0):
	return showPolls(PID)

#*******************************************************************************
@put('/polls<PID>')
def __putPoll():
    return "Hello World!"

#*******************************************************************************
@delete('/polls<PID>')
def __delPoll():
    return "Hello World!"


#a#*****************************************************************************
@post('/polls/<PID>/votes')
def __postVote():
	obj = {}
	obj['pid'] = PID
	obj['appointment'] =request.forms.get('appointment')
	return "Hello World!"

#*******************************************************************************
@get('/polls/<PID>/votes/<VID>')
def __getVotes(PID=0, VID=0):
	obj = {}
	obj['pid'] = PID
	obj['vid'] = VID
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
