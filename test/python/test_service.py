#!/usr/bin/env python
import os
import sys
import time

# Assumes we're running from the test/python dir
sys.path.append('../../build')
sys.path.append('../../build/swig')
import quickmsg_py

class ServiceImpl(quickmsg_py.Service):
    def __init__(self, *args, **kwargs):
        super(ServiceImpl, self).__init__(*args, **kwargs)
        self.msg_count = 0

    def service_impl(self, req):
        print 'Python inherited service callback'
        print 'Request:', req
        self.msg_count += 1
        return "World {0}".format(self.msg_count)

if __name__=='__main__':
    quickmsg_py.init("test_py_service")
    svc = ServiceImpl('hello')
    svc.spin()
    del svc
    

