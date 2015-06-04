%module(directors="1") quickmsg_java
%include "swig_includes.i"

//%shared_ptr(quickmsg::Message) 
//%shared_ptr(quickmsg::ServiceReply)

%feature("director") Service;
%feature("director") AsyncSubscriber;

// support for shared pointers
//%template(PeerPtr) boost::shared_ptr<Peer>;
//%template(ServiceReplyPtr) boost::shared_ptr<ServiceReply>;
//%template(MessagePtrVecPtr) boost::shared_ptr<std::vector<boost::shared_ptr<Message> > >;

%{
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

//%shared_ptr(quickmsg::Message);
//%shared_ptr(quickmsg::Peer);
//%shared_ptr(quickmsg::ServiceReply);
//%shared_ptr(quickmsg::PeerList);
//%shared_ptr(quickmsg::MsgList);
%template(MessageVec) std::vector<quickmsg::Message*>;
%template(PeerVec) std::vector<quickmsg::Peer*>;

/* typedef boost::shared_ptr<quickmsg::Message> quickmsg::MessagePtr; */
/* typedef boost::shared_ptr<quickmsg::ServiceReply> quickmsg::ServiceReplyPtr; */
/* typedef std::vector<quickmsg::MessagePtr> quickmsg::MsgList; */
/* typedef boost::shared_ptr<quickmsg::MsgList> quickmsg::MsgListPtr; */
/* typedef boost::shared_ptr<quickmsg::Peer> quickmsg::PeerPtr; */
/* typedef std::vector<quickmsg::PeerPtr> quickmsg::PeerList; */
/* typedef boost::shared_ptr<quickmsg::PeerList> quickmsg::PeerListPtr; */

// callback definitions
%{ 
  void java_MessageCallback(const Message* msg, void* args);
  const char* java_ServiceCallback(const Message* msg, void* args);
  %}
 
%typemap(jstype) quickmsg::MessageCallback cb "IMessageCallback";
%typemap(jtype) quickmsg::MessageCallback cb "IMessageCallback";
%typemap(jni) quickmsg::MessageCallback cb "jobject";
%typemap(javain) quickmsg::MessageCallback cb "$javainput";
// 4:
%typemap(in,numinputs=1) (quickmsg::MessageCallback cb, void *args) 
{
  java_cb_data* data = (java_cb_data*)malloc(sizeof(java_cb_data));
  data->env = jenv;
  data->obj = JCALL1(NewGlobalRef, jenv, $input);
  JCALL1(DeleteLocalRef, jenv, $input);
  $1 = java_MessageCallback;
  $2 = data;
}
%typemap(freearg) (quickmsg::MessageCallback cb, void* args) 
{
  free($2);
}

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
/* %typemap(freearg) (quickmsg::ServiceCallback cb, void* args)  */
/* { */
/*   free($2); */
/* } */
// Handle exceptions
%typemap(throws, throws="quickmsg.ServiceCallTimeout") quickmsg::ServiceCallTimeout %{
  jclass excep = jenv->FindClass("quickmsg/ServiceCallTimeout");
  if (exep) {
    jenv->ThrowNew(excep, $1.what().c_str());
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
