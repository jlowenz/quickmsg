#!/usr/bin/env python

import sys
import time 

# Assumes we're running from the test/python dir
sys.path.append('../../build')
sys.path.append('../../build/swig')
import quickmsg_py

if __name__=='__main__':
    # By default, qm uses whatever interface it finds finds first.
    # this should be tailored to network configuration
    iface = ""
    quickmsg_py.init("test_py_client", iface)
    c = quickmsg_py.Client('hello')

    req_msg = "Hello"
    
    for i in xrange(10):
        print 'Python client requesting:', req_msg    
        resp_msg = c.calls(req_msg)
        print "Service replied:", resp_msg
        time.sleep(0.5)
        
    del c
