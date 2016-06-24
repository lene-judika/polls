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
    print('dbCall')
    print(c.fetchall())
    return c.fetchall()

#*******************************************************************************
def stmtNextID(id, table):
    stmt = "select max(" + id + ") as max from " + table
    r = dbCall(stmt)[0]['max']
    print(r)
    if (r is None):
        r = 1
    return r

#*******************************************************************************
def postPoll(obj):
    print obj
    pid = stmtNextID('pid', 'polls')
    print()
    n = obj['name']
    app = obj['appointments']
    pw = obj['password']
    stmt = "insert into polls values ({0}, '{1}', '{2}')".format(pid, pw, n)
    print(stmt)
    ret = dbCall(stmt)
    print(ret)
    aid = stmtNextID('aid', 'appointments')
    for date in app:
        stmt = "insert into appointments values({0}, {1}, '{2}')".format(aid, pid, date)
        dbCall(stmt)
