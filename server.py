#!/usr/bin/python
from bottle import route, run, request, get, post, put, delete, response
import sqlite3
import dbio
import json

#*******************************************************************************
def getRequestObj():
	obj = request.body.read()
	obj = json.loads(obj)
	for key, value in obj.iteritems():
		print ("{0}: {1}".format(key, value))
		obj[key]=value
	return obj

#*******************************************************************************
@post('/polls')
def __postPoll():
	obj = getRequestObj()
	# Ueberpruefung der Eingaben ...
	return dbio.postPoll(obj)

#*******************************************************************************
@get('/polls/<PID>')
def __getPolls(PID=0):
	return dbio.showPoll(PID)

#*******************************************************************************
@put('/polls/<PID>')
def __putPoll( PID=0 ):
	obj = getRequestObj()
	obj['pid'] = PID
	return dbio.putPoll(obj)

#*******************************************************************************
@delete('/polls/<PID>')
def __delPoll ( PID=0 ) :
	obj = getRequestObj()
	obj['pid'] = PID
	print (obj)
	r = dbio.deletePoll(obj)
	if r is None:
		response.status = 204
	return r


#a#*****************************************************************************
@post('/polls/<PID>/votes')
def __postVote(PID=0):
	obj = getRequestObj()
	obj['pid'] = PID
	print (obj)
	return dbio.postVote(obj)

#*******************************************************************************
@get('/polls/<PID>/votes/<VID>')
def __getVotes(PID=0, VID=0):
	obj = {}
	obj['pid'] = PID
	obj['vid'] = VID
	return dbio.getVote(obj)

#*******************************************************************************
@put('/polls/<PID>/votes/<VID>')
def __putVote(PID=0, VID=0):
	obj = getRequestObj()
	obj['pid'] = PID
	obj['vid'] = VID
	return dbio.putVote(obj)

#*******************************************************************************
@delete('/polls/<PID>/votes/<VID>')
def __delVote(PID=0, VID=0):
	obj = {}
	obj['pid'] = PID
	obj['vid'] = VID
	r = dbio.deleteVote(obj)
	if r is None:
		response.status = 204
	return r


#*******************************************************************************
run(host='localhost', port=8080, debug=True)
