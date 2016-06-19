#!/usr/bin/python

import sqlite3



#*******************************************************************************
def dict_factory(cursor, row):
    d = {}
    for idx, col in enumerate(cursor.description):
        d[col[0]] = row[idx]
    return d

#*******************************************************************************
def stmtNextID(id, table):
	stmt = "select max(" + id + ") as max from " + table
    return stmt

#*******************************************************************************
def dbCall(stmt):
	conn = sqlite3.connect('polls.db')
	conn.row_factory = dict_factory
	c = conn.cursor()
	c.execute(stmt)
	return c.fetchall()
