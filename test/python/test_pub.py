#!/usr/bin/env python

import sys
import json
import numpy as np
import time

sys.path.append('../../build')
sys.path.append('../../build/swig')
import quickmsg_py

if __name__=='__main__':
    # By default, qm uses whatever interface it finds finds first.
    # this should be tailored to network configuration
    iface = ""
    quickmsg_py.init("test_py_pub", iface)
    p = quickmsg_py.Publisher('chatter')

    for i in xrange(10):
        if not quickmsg_py.ok():
            break
        some_msg = json.dumps({'important_matrix':np.random.rand(4,4).tolist()})
        print 'publishing', some_msg
        p.publish(some_msg)
        time.sleep(0.5)
    

