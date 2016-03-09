%module(directors="1") quickmsg_java
%include "swig_includes.i"

%feature("director") Service;
%feature("director") AsyncSubscriber;

%{
#define SWIG_JAVA_ATTACH_CURRENT_THREAD_AS_DAEMON
#include "quickmsg/quickmsg.hpp"
#include "quickmsg/types.hpp"
#include "quickmsg/publisher.hpp"
#include "quickmsg/subscriber.hpp"
#include "quickmsg/service.hpp"
#include "quickmsg/client.hpp"
#include "quickmsg/group_node.hpp"
#include "quickmsg/quickmsg_java.hpp"
using namespace quickmsg;
%}

#pragma SWIG nowarn=401


%template(MessageVec) std::vector<quickmsg::Message*>;
%template(PeerVec) std::vector<quickmsg::Peer*>;

// callback definitions
%{
  void java_MessageCallback(const Message* msg, void* args);
  char* java_ServiceCallback(const Message* msg, void* args);
  %}

%typemap(jstype) quickmsg::MessageCallback cb "IMessageCallback";
%typemap(jtype) quickmsg::MessageCallback cb "IMessageCallback";
%typemap(jni) quickmsg::MessageCallback cb "jobject";
%typemap(javain) quickmsg::MessageCallback cb "$javainput";
// 4:
%typemap(in,numinputs=1) (quickmsg::MessageCallback cb, void *args)
{
  // TODO: consider making java_cb_data a java class so it can be
  // garbage collected
  java_cb_data* data = new java_cb_data;
  data->env = jenv;
  data->obj = JCALL1(NewGlobalRef, jenv, $input);
  JCALL1(DeleteLocalRef, jenv, $input);
  $1 = java_MessageCallback;
  $2 = data;
}
/* %typemap(freearg) (quickmsg::MessageCallback cb, void* args)  */
/* { */
/*   free($2); */
/* } */

%typemap(jstype) quickmsg::ServiceCallback cb "IServiceCallback";
%typemap(jtype) quickmsg::ServiceCallback cb "IServiceCallback";
%typemap(jni) quickmsg::ServiceCallback cb "jobject";
%typemap(javain) quickmsg::ServiceCallback cb "$javainput";
// 4:
%typemap(in,numinputs=1) (quickmsg::ServiceCallback cb, void *args)
{
  java_cb_data* data = (java_cb_data*)malloc(sizeof(java_cb_data));
  data->env = jenv;
  data->obj = JCALL1(NewGlobalRef, jenv, $input);
  JCALL1(DeleteLocalRef, jenv, $input);
  $1 = java_ServiceCallback;
  $2 = data;
}
/* This is NOT how to clean up the memory - can only happen when the handler is removed
/* %typemap(freearg) (quickmsg::ServiceCallback cb, void* args)  */
/* { */
/*   free($2); */
/* } */
// Handle exceptions
%typemap(throws, throws="quickmsg.ServiceCallTimeout") quickmsg::ServiceCallTimeout %{
  jclass excep = jenv->FindClass("quickmsg/ServiceCallTimeout");
  if (excep) {
    jenv->ThrowNew(excep, $1.what());
  }
  return $null;
%}
%typemap(javabase) quickmsg::ServiceCallTimeout "java.lang.Exception";
%typemap(javacode) quickmsg::ServiceCallTimeout %{
  public String getMessage() {
    return what();
  }
%}


%include "quickmsg/quickmsg.hpp"
%include "quickmsg/types.hpp"
%include "quickmsg/publisher.hpp"
%include "quickmsg/subscriber.hpp"
%include "quickmsg/service.hpp"
%include "quickmsg/client.hpp"
%include "quickmsg/group_node.hpp"
