#include <boost/log/trivial.hpp>
#include <quickmsg/group_node.hpp>
#include <quickmsg/client.hpp>
#include <quickmsg/quickmsg.hpp>
#include <thread>
#include <chrono>

namespace quickmsg {

  void client_handler(const Message* msg, void* args)
  {
    static_cast<Client*>(args)->handle_response(msg);
  }

  Client::Client(const std::string& srv_name) : 
    srv_name_(srv_name)
  {
    message_received_ = false;
    // create the group node
    std::string name("C/");
    node_ = new GroupNode(name + srv_name);
    node_->register_whispers(&client_handler, (void*)this);
    node_->join(srv_name_);
    node_->async_spin();
    BOOST_LOG_TRIVIAL(debug) << "waiting for server" << std::endl;
    node_->wait_join(srv_name_); // wait for service
    BOOST_LOG_TRIVIAL(debug) << "server joined group" << std::endl;
  }

  Client::~Client()
  {    
    node_->leave(srv_name_);
    node_->stop();
    delete node_;
  }


  std::string
  Client::calls(const std::string& msg, int timeout_s) throw(ServiceCallTimeout)
  {
    ServiceReplyPtr resp = call(msg, timeout_s);    
    return resp->msg;
  }
  
  ServiceReplyPtr
  Client::call(const std::string& req, int timeout_s) throw(ServiceCallTimeout)
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
    
    if (!response_) {
      throw InvalidResponse();
    }
    if (!message_received_.load()) {
      throw ServiceCallTimeout();
    }

    return response_;
  }

  // TODO: shouldn't this be a ServiceReply? how else is failure indicated?
  void 
  Client::handle_response(const Message* resp)
  {
    assert(resp);
    if (resp->msg) {
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
