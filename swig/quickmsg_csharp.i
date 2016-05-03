%module(directors="1") quickmsg_csharp
%include "swig_includes.i"
%ignore quickmsg::Message::msg;

%{
#include "quickmsg/quickmsg.hpp"
#include "quickmsg/types.hpp"
#include "quickmsg/publisher.hpp"
#include "quickmsg/subscriber.hpp"
#include "quickmsg/service.hpp"
#include "quickmsg/client.hpp"
using namespace quickmsg;
%}

%feature("director") Service;
%feature("director") AsyncSubscriber;

%include "quickmsg/quickmsg.hpp"
%include "quickmsg/types.hpp"
%include "quickmsg/publisher.hpp"
%include "quickmsg/subscriber.hpp"
%include "quickmsg/service.hpp"
%include "quickmsg/client.hpp"
