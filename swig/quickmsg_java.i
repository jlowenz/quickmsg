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
using namespace quickmsg;
%}

%shared_ptr(quickmsg::Message);
%shared_ptr(quickmsg::Peer);
%shared_ptr(quickmsg::ServiceReply);
%shared_ptr(quickmsg::PeerList);
%shared_ptr(quickmsg::MsgList);
%template(MessageVec) std::vector<quickmsg::Message*>;
%template(PeerVec) std::vector<quickmsg::Peer*>;

/* typedef boost::shared_ptr<quickmsg::Message> quickmsg::MessagePtr; */
/* typedef boost::shared_ptr<quickmsg::ServiceReply> quickmsg::ServiceReplyPtr; */
/* typedef std::vector<quickmsg::MessagePtr> quickmsg::MsgList; */
/* typedef boost::shared_ptr<quickmsg::MsgList> quickmsg::MsgListPtr; */
typedef boost::shared_ptr<quickmsg::Peer> quickmsg::PeerPtr;
/* typedef std::vector<quickmsg::PeerPtr> quickmsg::PeerList; */
/* typedef boost::shared_ptr<quickmsg::PeerList> quickmsg::PeerListPtr; */
	
%include "quickmsg/quickmsg.hpp"
%include "quickmsg/types.hpp"
%include "quickmsg/publisher.hpp"
%include "quickmsg/subscriber.hpp"
%include "quickmsg/service.hpp"
%include "quickmsg/client.hpp"
%include "quickmsg/group_node.hpp"



