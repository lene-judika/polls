#!/usr/bin/python

import sqlite3
from bottle import response
from datetime import datetime

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
        print ("in try Y-m-d ")
        print datestr
        datetime.strptime(datestr, '%Y-%m-%d')
        return True
    except :
        try:
            print ("in try Y-m-d H:M ")
            datetime.strptime(datestr, '%Y-%m-%d %H:%M')
            return True
        except:
            try:
                print ("in try Y-m-dTH:M ")
                datetime.strptime(datestr, '%Y-%m-%dT%H:%M')
                return True
            except:
                try:
                    print ("in try Y-m-dTH:M:S")
                    datetime.strptime(datestr, '%Y-%m-%dT%H:%M:%S')
                    return True
                except:
                    try:
                        print ("in try Y-m-d H:M:S")
                        datetime.strptime(datestr, '%Y-%m-%d %H:%M:%S')
                        return True
                    except:
                        return  False

#*******************************************************************************
def getAIDfromDate(date, pid):
    stmt = "select aid \
            from appointments \
            where pid={0} and date=datetime('{1}')".format(pid, date)
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
def insAppointments(apps,pid):
    try:
        for date in apps:
            print (date)
            aid = stmtNextID('aid', 'appointments')
            stmt = "insert into appointments(aid, pid, date) \
                    select {0},{1}, datetime('{2}') \
                    where not exists( \
                    select 1,2,3 from appointments \
                    where pid={1} and date=datetime('{2}'))".format(
                    aid, pid, date)
            print(stmt)
            r = dbCall(stmt)
            print(r)
    except:
        response.status = 400
        return  {'statuscode':400,
                'reason': 'Syntax error datetime format'}


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
        return  {'statuscode':400,
                'reason': 'Syntax error or missing key in your request'}
    if (len(pw) < 8):
        response.status = 400
        return {'statuscode':400,
                'reason': 'the password is waaayyy tooo short. I mean: seriously?'}
    #check, if dates are valid:
    for d in apps:
        if not validateDateFormat(d):
            response.status = 400
            return  {'statuscode':400,
                    'reason': 'Date or datetime format is not valid'}

    # if everything is valid, insertions can be done:
    stmt = "insert into polls values ({0}, '{1}', '{2}')".format(pid, pw, n)
    ret = dbCall(stmt)
    insAppointments(apps, pid)

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
        return  {'statuscode':400,
                'reason': 'Syntax error or missing key in your request'}
    if  not isInt(pid) or not validateID('pid', pid, 'polls'):
        response.status = 404
        return {'statuscode':404,
                'reason': "this PID does not exist.'"}
    #check, if dates are valid:
    for d in apps:
        if not validateDateFormat(d):
            response.status = 400
            return  {'statuscode':400,
                    'reason': 'Date or datetime format is not valid'}

    # update polls table
    stmt = "update polls \
            set name = '{0}', password = '{1}' \
            where pid={2}".format(name, pwd, pid )
    r = dbCall(stmt)
    print(r)

    # set old appointments empty
    placeholder= '?' # For SQLite. See DBAPI paramstyle.
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
        return {'statuscode':404,
                'reason': "this PID does not exist.'"}
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
    obj['vid'] = vid
    pid = obj['pid']
    if not isInt(pid) or not validateID('pid', pid, 'polls'):
        response.status = 404
        return {'statuscode':404,
                'reason': "this PID does not exist.'"}
    # get new date
    try:
        date = obj['appointment']
    except:
        response.status = 400
        return  {'statuscode':400,
                'reason': "Syntax error: could not find key 'appointment'"}

    if not validateDateFormat(date):
        response.status = 400
        return {'statuscode':400,
                'reason': "Date or datetime format is not valid"}

    aid = getAIDfromDate(date, pid)
    if aid is None:
        response.status = 409
        return {'statuscode':409,
                'reason': "Date does not exist."}

    stmt = "insert into votes values({0}, {1}, {2})".format(vid, pid, aid)
    print(stmt)
    ret = dbCall(stmt)
    print(ret)

    response.status = 201
    return getVote(obj)

#*******************************************************************************
def deletePoll(obj):

    pid = obj['pid']
    if not isInt(pid) or not validateID('pid', pid, 'polls'):
        response.status = 404
        return {'statuscode':404,
                'reason': "this PID does not exist.'"}
    try:
        pwd = obj['password']
    except:
        response.status = 400
        return  {'statuscode':400,
                'reason': 'Syntax error or missing password'}

    stmt = "select pid from polls where pid={0} and password='{1}'".format(pid, pwd)
    r = dbCall(stmt)
    print ("return pwd Call: {0}".format(r))
    # delete only, if query returned  at least one row
    if r is None:
        response.status = 403
        return  {'statuscode':403,
                'reason': 'password invalid, sry'}
    else:
        stmt = "delete from votes where pid={0}".format(pid)
        ret = dbCall(stmt)
        print (ret)

        stmt = "delete from appointments where pid={0}".format(pid)
        ret = dbCall(stmt)
        print (ret)

        stmt = "delete from polls where pid={0}".format(pid)
        ret = dbCall(stmt)
        print (ret)
        response.status = 204 #empty body

#*******************************************************************************
def getVote(obj):
    vid = obj['vid']
    if not isInt(vid) or not validateID('vid', vid, 'votes'):
        response.status = 404
        return {'statuscode':404,
                'reason': "this VID does not exist.'"}
    pid = obj['pid']
    if not isInt(pid) or not validateID('pid', pid, 'polls'):
        response.status = 404
        return {'statuscode':404,
                'reason': "this PID does not exist.'"}

    stmt = "select a.date, v.vid \
            from appointments a, votes v\
            where v.vid={0} and a.pid={1} and v.aid=a.aid".format(vid,pid)
    r = dbCall(stmt)
    print(r)
    return r[0]

#*******************************************************************************
def deleteVote(obj):
        vid = obj['vid']
        if not isInt(vid) or not validateID('vid', vid, 'votes'):
            response.status = 404
            return {'statuscode':404,
                    'reason': "this VID does not exist.'"}
        pid = obj['pid']
        if not isInt(pid) or not validateID('pid', pid, 'polls'):
            response.status = 404
            return {'statuscode':404,
                    'reason': "this PID does not exist.'"}

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
        return  {'statuscode':400,
                'reason': "Syntax error: could not find key 'appointment'"}

    pid = obj['pid']
    if not isInt(pid) or not validateID('pid', pid, 'polls'):
        response.status = 404
        return {'statuscode':404,
                'reason': "this PID does not exist.'"}

    vid = obj['vid']
    if not isInt(vid) or not validateID('vid', vid, 'votes'):
        response.status = 404
        return {'statuscode':404,
                'reason': "this VID does not exist.'"}

    aid = getAIDfromDate(date,pid)
    if aid is None:
        response.status = 409
        return {'statuscode':409,
                'reason': "Date does not exist."}

    # update votes table
    stmt = "update votes \
            set aid = '{0}' \
            where vid={1}".format(aid, vid )
    r = dbCall(stmt)
    print(r)
    return getVote(obj)
