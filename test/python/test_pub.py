#!/usr/bin/env python

import sys
import json
import numpy as np
import time

sys.path.append('../../build')
sys.path.append('../../build/swig')
import quickmsg

if __name__=='__main__':
    p = quickmsg.Publisher('chatter')

    for i in range(10):
        some_msg = json.dumps({'important_matrix':np.random.rand(4,4).tolist()})
        print 'publishing', some_msg
        p.publish(some_msg)
        time.sleep(1)
    

