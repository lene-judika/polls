#!/usr/bin/python

NO_PID = {'statuscode':404, 'reason': "this PID does not exist."}

NO_VID = {'statuscode':404, 'reason': "this VID does not exist (at least not for this PID...)."}

SYNTAX_DATE = {'statuscode':400, 'reason': 'Syntax error datetime format'}

SYNTAX = {'statuscode':400, 'reason': 'Syntax error or missing key in your request'}

INVALID_PWD = {'statuscode':403, 'reason': 'password invalid, sry'}

SHORT_PWD = {'statuscode':400, 'reason': 'the password is waaayyy tooo short.'}

NO_DATE = {'statuscode':409, 'reason': "Date does not exist."}

NO_JSON = {'statuscode':400, 'reason': 'Syntax error: no correct json'}

NOT_FOUND = {'statuscode':404, 'reason': 'Nothing here...'}

NOT_ALLOWED = {'statuscode':405, 'reason': 'Method not allowed, sry'}
