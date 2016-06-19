#!/usr/bin/python

import sqlite3



#*******************************************************************************
def dict_factory(cursor, row):
    d = {}
    for idx, col in enumerate(cursor.description):
        d[col[0]] = row[idx]
    return d

#*******************************************************************************
def dbCall(stmt):
	conn = sqlite3.connect('polls.db')
	conn.row_factory = dict_factory
	c = conn.cursor()
	c.execute(stmt)
	return c.fetchall()

#*******************************************************************************
def stmtNextID(id, table):
	stmt = "select max(" + id + ") as max from " + table
    r = dbCall(stmt)
    return r[0]

#*******************************************************************************
def postPoll(name, appointments, password):
    pid = stmtNextID('pid', 'polls')
    stmt = "insert into polls values ({0}, '{1}', '{2}')".format(
        pid, password, name)
    ret = dbCall(stmt)
