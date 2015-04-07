#!/usr/bin/env python

import sys
import json
import numpy as np
import time

sys.path.append('/home/phil/devel/quickmsg/build')
sys.path.append('/home/phil/devel/quickmsg/build/swig')
import quickmsg

if __name__=='__main__':
    msg_queue_sz = 20
    s = quickmsg.Subscriber('chatter', msg_queue_sz)
    print 'entering main loop'

    while True:
        time.sleep(1)
    

