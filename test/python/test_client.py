#!/usr/bin/env python

import sys
import json

sys.path.append('../../build')
sys.path.append('../../build/swig')
import quickmsg_py

if __name__=='__main__':
    c = quickmsg_py.Client('add')

    req_msg = json.dumps({'ints_to_add':[1,2,3,4]})
    print 'requesting', req_msg
    
    resp_msg = c.calls(req_msg)
