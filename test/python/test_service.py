#!/usr/bin/env python
import os
import sys
import json
import numpy as np
import time

sys.path.append('../../build')
sys.path.append('../../build/swig')
import quickmsg

class ServiceImpl(quickmsg.Service):
    def __init__(self, *args, **kwargs):
        super(ServiceImpl, self).__init__(*args, **kwargs)

    def service_impl(self, req):
        print 'Python inherited service callback'
        msg=json.loads(req)
        print 'got request', msg
        result = np.sum(msg['ints_to_add'])
        return json.dumps({'ints_to_add:' : msg['ints_to_add'], 
                           'result' : result})

if __name__=='__main__':
    svc = ServiceImpl('add')
    svc.spin()
    
    

