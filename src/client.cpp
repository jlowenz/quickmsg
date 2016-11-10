#include <boost/log/trivial.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <quickmsg/group_node.hpp>
#include <quickmsg/client.hpp>
#include <quickmsg/quickmsg.hpp>
#include <thread>
#include <chrono>
#include <iostream>

namespace quickmsg {

  using namespace boost::uuids;
  
  void client_handler(const Message* msg, void* args)
  {
    static_cast<Client*>(args)->handle_response(msg);
  }

  Client::Client(const std::string& srv_name, bool wait) : 
    srv_name_(srv_name)
  {
    assert(ok() && "quickmsg shutdown or quickmsg::init() must be called first");
    message_received_ = false;
    // create the group node
    uuid cid = random_generator()();
    std::string name("C/");
    node_ = new GroupNode(name + to_string(cid) + "/" + srv_name);
    node_->register_whispers(&client_handler, (void*)this);
    node_->async_spin();

    if (wait) {
      BOOST_LOG_TRIVIAL(debug) << "waiting for server" << std::endl;
      node_->wait_join(srv_name_); // wait for service
      BOOST_LOG_TRIVIAL(debug) << "server joined group" << std::endl;
    } else {
      node_->join(srv_name_);      
    }
  }

  Client::~Client()
  {    
    node_->leave(srv_name_);
    //node_->stop();
    delete node_;
  }


  std::string
  Client::calls(const std::string& msg, int timeout_s)
  {
    ServiceReplyPtr resp = call(msg, timeout_s);    
    return resp->msg;
  }
  
  ServiceReplyPtr
  Client::call(const std::string& req, int timeout_s)
  {
    message_received_.store(false);
    response_.reset();
    node_->shout(srv_name_, req);
    { 
      // wait for the response
      std::unique_lock<std::mutex> lock(response_mutex_);
      response_cond_.wait_for(lock, std::chrono::seconds(timeout_s), 
			      [&]{ return !ok() || message_received_.load(); });
    }
    
    if (!message_received_.load() && ok()) {
      throw ServiceCallTimeout();
    }
    if (!response_ && ok()) {
      throw InvalidResponse();
    }

    return response_;
  }

  // TODO: shouldn't this be a ServiceReply? how else is failure indicated?
  void 
  Client::handle_response(const Message* resp)
  {
    BOOST_LOG_TRIVIAL(debug) << "Client::handle_response" << resp << std::endl;
    assert(resp);
    if (resp->msg != "") {
      BOOST_LOG_TRIVIAL(debug) << "received response\n" << resp->msg << std::endl;
      ServiceReplyPtr rep(new ServiceReply(*resp, true));
      response_ = rep;
      message_received_.store(true);
      response_cond_.notify_one();
    } else {
      message_received_.store(false);
      response_cond_.notify_one();
    }
  }


}
