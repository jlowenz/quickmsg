%module(directors="1") quickmsg
%include "swig_includes.i"

%{
#include "quickmsg/client.hpp"
#include "quickmsg/publisher.hpp"
#include "quickmsg/subscriber.hpp"
#include "quickmsg/service.hpp"
using namespace quickmsg;
%}

%feature("director") Service;
%feature("director") Subscriber;
%feature("director") AsyncSubscriber;

%include "quickmsg/publisher.hpp"
%include "quickmsg/subscriber.hpp"
%include "quickmsg/service.hpp"
%include "quickmsg/client.hpp"
%include "quickmsg/types.hpp"


 // %include "quickmsg/group_node.hpp"


