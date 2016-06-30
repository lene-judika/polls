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
    conn.commit()
    return c.fetchall()

#*******************************************************************************
def dbCallMany(stmt, params):
    conn = sqlite3.connect('polls.db')
    conn.row_factory = dict_factory
    c = conn.cursor()
    c.execute(stmt, params)
    conn.commit()
    return c.fetchall()

#*******************************************************************************
def stmtNextID(id, table):
    stmt = "select max(" + id + ") as max from " + table
    r = dbCall(stmt)[0]['max']
    if (r is None):
        r = 0
    return r +1

#*******************************************************************************
def insAppointments(obj,pid):
    app = obj['appointments']
    for date in app:
        aid = stmtNextID('aid', 'appointments')
        stmt = "insert into appointments values({0}, {1}, '{2}')".format(aid, pid, date)
        dbCall(stmt)
    pass

#*******************************************************************************
def postPoll(obj):

    ### generate next poll-ID PID
    pid = stmtNextID('pid', 'polls')

    ### insert new poll
    n = obj['name']
    pw = obj['password']
    stmt = "insert into polls values ({0}, '{1}', '{2}')".format(pid, pw, n)
    ret = dbCall(stmt)
    insAppointments(obj, pid)

    return showPoll(pid)

#*******************************************************************************
def putPoll(obj):
    pid = obj['pid']
    name = obj['name']
    pwd = obj['password']
    # update polls table
    stmt = "update polls \
            set name = '{0}', password = '{1}' \
            where pid={2}".format(name, pwd, pid )
    r = dbCall(stmt)
    print(r)

    # set old appointments empty
    placeholder= '?' # For SQLite. See DBAPI paramstyle.
    placeholders= ', '.join(placeholder for unused in obj['appointments'])
    stmt= "update appointments \
            set date='' \
            where pid={0} and date not in ({1})".format(pid, placeholders)
    print(stmt)
    print(obj['appointments'])
    dbCallMany(stmt, obj['appointments'])

    return showPoll(pid)

#*******************************************************************************
def showPoll(pid):
    ### create return-data
    stmt = "select p.PID, p.name, a.aid, a.date  \
            from polls p, appointments a \
            where p.pid={0} and a.pid={1}".format(pid,pid)
    r = dbCall(stmt)
    applist = []
    retdict = {}
    for row in r:
        applist.append(row['date'])
    retdict['pid'] = r[0]['pid']
    retdict['name'] = r[0]['name']
    retdict['appointments'] = applist

    # votes ermitteln
    stmt = "select a.date as date, count(vid) as cnt \
            from appointments a, votes v \
            where a.aid=v.aid and a.pid={0} \
            group by(a.aid)".format(retdict['pid'])
    r = dbCall(stmt)
    votes = {}
    for row in r:
        votes[row['date']] = row['cnt']
    retdict['votes'] = votes

    return retdict

#*******************************************************************************
def postVote(obj):
    vid = stmtNextID('vid', 'votes')

    pid = obj['pid']
    date = obj['appointment']
    stmt = "select aid \
            from appointments \
            where pid={0} and date='{1}'".format(pid, date)
    print(stmt)
    aid = dbCall(stmt)[0]['aid']
    print(aid)

    stmt = "insert into votes values({0}, {1}, {2})".format(vid, pid, aid)
    print(stmt)

    ret = dbCall(stmt)
    print(ret)

#*******************************************************************************
def deletePoll(obj):

    pwd = obj['password']
    pid = obj['pid']

    stmt = "select pid from polls where pid={0} and password='{1}'".format(pid, pwd)
    r = dbCall(stmt)
    print ("return pwd Call: {0}".format(r))
    # delete only, if query returned  at least one row
    if r:
        stmt = "delete from votes where pid={0}".format(pid)
        ret = dbCall(stmt)
        print (ret)

        stmt = "delete from appointments where pid={0}".format(pid)
        ret = dbCall(stmt)
        print (ret)

        stmt = "delete from polls where pid={0}".format(pid)
        ret = dbCall(stmt)
        print (ret)

#*******************************************************************************
def getVote(obj):
    pid = obj['pid']
    vid = obj['vid']

    stmt = "select a.date, v.vid \
            from appointments a, votes v\
            where v.vid={0} and a.pid={1}".format(vid,pid)
    r = dbCall(stmt)
    print(r)
    return r[0]
