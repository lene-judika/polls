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
    print('dbCall stmt:')
    print (stmt)
    conn = sqlite3.connect('polls.db')
    conn.row_factory = dict_factory
    c = conn.cursor()
    c.execute(stmt)
    conn.commit()
    return c.fetchall()

#*******************************************************************************
def stmtNextID(id, table):
    stmt = "select max(" + id + ") as max from " + table
    r = dbCall(stmt)[0]['max']
    if (r is None):
        r = 1
    return r

#*******************************************************************************
def postPoll(obj):
    print obj
    ### generate next poll-ID PID
    pid = stmtNextID('pid', 'polls')

    ### insert new poll
    n = obj['name']
    pw = obj['password']
    stmt = "insert into polls values ({0}, '{1}', '{2}')".format(pid, pw, n)
    ret = dbCall(stmt)

    ### insert new appointments
    app = obj['appointments']
    for date in app:
        aid = stmtNextID('aid', 'appointments')
        stmt = "insert into appointments values({0}, {1}, '{2}')".format(aid, pid, date)
        dbCall(stmt)

    ### create return-data
    stmt = "select p.PID, p.name, a.aid, a.date \
            from polls p, appointments a\
            where p.pid={0} and a.pid={1}".format(pid,pid)
    r = dbCall(stmt)
    applist = []
    retdict = {}
    for row in r:
        applist.append(row['date'])
    retdict['pid'] = r[0]['pid']
    retdict['name'] = r[0]['name']
    retdict['appointments'] = applist

    return retdict
