%module(directors="1") quickmsg_java
%include "swig_includes.i"

%{
#include "quickmsg/quickmsg.hpp"
#include "quickmsg/types.hpp"
#include "quickmsg/publisher.hpp"
#include "quickmsg/subscriber.hpp"
#include "quickmsg/service.hpp"
#include "quickmsg/client.hpp"
using namespace quickmsg;
%}

//%shared_ptr(quickmsg::Message)
//%shared_ptr(quickmsg::ServiceReply)

%feature("director") Service;
%feature("director") Subscriber;
%feature("director") AsyncSubscriber;

%include "quickmsg/quickmsg.hpp"
%include "quickmsg/types.hpp"
%include "quickmsg/publisher.hpp"
%include "quickmsg/subscriber.hpp"
%include "quickmsg/service.hpp"
%include "quickmsg/client.hpp"



