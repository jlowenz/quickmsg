#!/usr/bin/env python

import sys
import json
import numpy as np
import time

sys.path.append('/home/phil/devel/quickmsg/build')
sys.path.append('/home/phil/devel/quickmsg/build/swig')
import quickmsg


def add_ints(req):
    msg=json.loads(req->msg)
    print 'got request', msg
    result = np.sum(msg['ints_to_add'])
    return json.dumps({'ints_to_add:' : msg['ints_to_add'], 
                       'result' : result})

if __name__=='__main__':
    svc = quickmsg.Service('chatter', add_ints)
    svc.spin()
    
    

