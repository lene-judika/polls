#!/usr/bin/env python2

import sqlite3
from bottle import response
from datetime import datetime

import err

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
def isInt(s):
    try:
        int(s)
        return True
    except:
        return False

#*******************************************************************************
def validateDateFormat(datestr):
    try:
        datetime.strptime(datestr, '%Y-%m-%d')
        return True
    except :
        try:
            datetime.strptime(datestr, '%Y-%m-%d %H:%M')
            return True
        except:
            try:
                datetime.strptime(datestr, '%Y-%m-%dT%H:%M')
                return True
            except:
                try:
                    datetime.strptime(datestr, '%Y-%m-%dT%H:%M:%S')
                    return True
                except:
                    try:
                        datetime.strptime(datestr, '%Y-%m-%d %H:%M:%S')
                        return True
                    except:
                        return  False

#*******************************************************************************
def validatePwd(pid, pwd):
    stmt = "select pid \
            from polls \
            where pid={0} and password='{1}'".format(pid, pwd)
    return dbCall(stmt) #implicit boolean


#*******************************************************************************
def getAIDfromDate(date, pid):
    stmt = "select aid \
            from appointments \
            where pid={0} and date=	strftime('%Y-%m-%dT%H:%M','{1}')".format(pid, date)
    r = dbCall(stmt)
    print r
    if not r:
        return None
    else:
        return r[0]['aid']


#*******************************************************************************
def validateID( col, id, table):
    stmt = "select count(*) as c from {0} where {1}={2}".format(table, col, id)
    print stmt
    return (dbCall(stmt)[0]['c'] == 1)


#*******************************************************************************
def stmtNextID(id, table):
    stmt = "select max(" + id + ") as max from " + table
    r = dbCall(stmt)[0]['max']
    if (r is None):
        r = 0
    return r +1


#*******************************************************************************
def insAppointments(apps,pid):
    # dates have to be valid at this point :)
    for date in apps:
        print (date)
        aid = stmtNextID('aid', 'appointments')
        stmt = "insert into appointments(aid, pid, date) \
                select {0},{1}, strftime('%Y-%m-%dT%H:%M','{2}') \
                where not exists( \
                select 1,2,3 from appointments \
                where pid={1} and date=strftime('%Y-%m-%dT%H:%M','{2}'))".format(
                aid, pid, date)
        print(stmt)
        r = dbCall(stmt)
        print(r)


#*******************************************************************************
def postPoll(obj):

    ### generate next poll-ID PID
    pid = stmtNextID('pid', 'polls')

    ### insert new poll
    try:
        n = obj['name']
        pw = obj['password']
        apps = obj['appointments']
    except:
        response.status = 400
        return  err.SYNTAX

    if (len(pw) < 8):
        response.status = 400
        return err.SHORT_PWD
    #check, if dates are valid:
    for d in apps:
        if not validateDateFormat(d):
            response.status = 400
            return err.SYNTAX_DATE

    # if everything is valid, insertions can be done:
    stmt = "insert into polls values ({0}, '{1}', '{2}')".format(pid, pw, n)
    ret = dbCall(stmt)
    insAppointments(apps, pid)
    response.status = 201
    return showPoll(pid)


#*******************************************************************************
def putPoll(obj):
    try:
        pid = obj['pid']
        name = obj['name']
        pwd = obj['password']
        apps = obj['appointments']
    except:
        response.status = 400
        return  err.SYNTAX

    if  not isInt(pid) or not validateID('pid', pid, 'polls'):
        response.status = 404
        return err.NO_PID

    #check, if dates are valid:
    for d in apps:
        if not validateDateFormat(d):
            response.status = 400
            return  err.SYNTAX_DATE

    # check if password is valid:
    if not validatePwd(pid, pwd):
        response.status = 403
        return  err.INVALID_PWD

    # update polls table
    stmt = "update polls \
            set name = '{0}', password = '{1}' \
            where pid={2}".format(name, pwd, pid )
    r = dbCall(stmt)
    print(r)

    # set old appointments empty
    placeholder= "strftime('%Y-%m-%dT%H:%M',?)" # For SQLite. See DBAPI paramstyle.
    placeholders= ', '.join(placeholder for unused in apps )
    stmt= "update appointments \
            set date='' \
            where pid={0} and date not in ({1})".format(pid, placeholders)
    try:
        dbCallMany(stmt, apps)
    except:
        response.status = 400
        return  {'statuscode':400,
                'reason': 'Syntax error: could not empty-string (verb) old appointments'}
    # insert new appointments
    insAppointments(apps, pid)

    return showPoll(pid)

#*******************************************************************************
def showPoll(pid):

    if not isInt(pid) or not validateID('pid', pid, 'polls'):
        response.status = 404
        return err.NO_PID

    ### create return-data
    stmt = "select p.pid as pid, p.name, a.aid, a.date  \
            from polls p, appointments a \
            where p.pid={0} and a.pid={1}".format(pid,pid)
    r = dbCall(stmt)
    applist = []
    retdict = {}
    for row in r:
        applist.append(row['date'])
    retdict['PID'] = r[0]['pid']
    retdict['name'] = r[0]['name']
    retdict['appointments'] = applist

    # detect votes
    stmt = "select a.date as date, count(vid) as cnt \
            from appointments a, votes v \
            where a.aid=v.aid and a.pid={0}  and a.date != '' \
            group by(a.aid)".format(retdict['PID'])
    r = dbCall(stmt)
    votes = {}
    for row in r:
        votes[row['date']] = row['cnt']
    retdict['votes'] = votes

    return retdict

#*******************************************************************************
def postVote(obj):
    vid = stmtNextID('vid', 'votes')
    obj['vid'] = vid
    pid = obj['pid']
    if not isInt(pid) or not validateID('pid', pid, 'polls'):
        response.status = 404
        return err.NO_PID

    # get new date
    try:
        date = obj['appointment']
    except:
        response.status = 400
        return  err.SYNTAX

    if not validateDateFormat(date):
        response.status = 400
        return err.SYNTAX_DATE

    aid = getAIDfromDate(date, pid)
    if aid is None:
        response.status = 409
        return err.NO_DATE

    stmt = "insert into votes values({0}, {1}, {2})".format(vid, pid, aid)
    print(stmt)
    ret = dbCall(stmt)

    response.status = 201
    return getVote(obj)

#*******************************************************************************
def deletePoll(obj):

    pid = obj['pid']
    if not isInt(pid) or not validateID('pid', pid, 'polls'):
        response.status = 404
        return err.NO_PID
    try:
        pwd = obj['password']
    except:
        response.status = 400
        return  err.SYNTAX

    # check if password is valid:
    if not validatePwd(pid, pwd):
        response.status = 403
        return  err.INVALID_PWD

    stmt = "delete from votes where pid={0}".format(pid)
    ret = dbCall(stmt)

    stmt = "delete from appointments where pid={0}".format(pid)
    ret = dbCall(stmt)

    stmt = "delete from polls where pid={0}".format(pid)
    ret = dbCall(stmt)

    response.status = 204 #empty body

#*******************************************************************************
def getVote(obj):
    vid = obj['vid']
    if not isInt(vid) or not validateID('vid', vid, 'votes'):
        response.status = 404
        return err.NO_VID

    pid = obj['pid']
    if not isInt(pid) or not validateID('pid', pid, 'polls'):
        response.status = 404
        return err.NO_PID

    stmt = "select a.date as appointment, v.vid as VID\
            from appointments a, votes v\
            where v.vid={0} and a.pid={1} and v.aid=a.aid".format(vid,pid)
    r = dbCall(stmt)
    return r[0]

#*******************************************************************************
def deleteVote(obj):
    vid = obj['vid']
    if not isInt(vid) or not validateID('vid', vid, 'votes'):
        response.status = 404
        return err.NO_VID

    pid = obj['pid']
    if not isInt(pid) or not validateID('pid', pid, 'polls'):
        response.status = 404
        return err.NO_PID

    stmt = "delete from votes where pid={0} and vid={1}".format(pid, vid)
    ret = dbCall(stmt)
    print (ret)
    response.status = 204
    return ret

#*******************************************************************************
def putVote(obj):
    try:
        date = obj['appointment']
    except:
        response.status = 400
        return err.SYNTAX

    pid = obj['pid']
    if not isInt(pid) or not validateID('pid', pid, 'polls'):
        response.status = 404
        return err.NO_PID

    vid = obj['vid']
    if not isInt(vid) or not validateID('vid', vid, 'votes'):
        response.status = 404
        return err.NO_VID

    aid = getAIDfromDate(date,pid)
    if aid is None:
        response.status = 409
        return err.NO_DATE

    # update votes table
    stmt = "update votes \
            set aid = '{0}' \
            where vid={1}".format(aid, vid )
    r = dbCall(stmt)
    print(r)
    return getVote(obj)
