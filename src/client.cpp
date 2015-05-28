#include <glog/logging.h>
#include <quickmsg/client.hpp>
#include <quickmsg/quickmsg.hpp>
#include <chrono>

namespace quickmsg {

  void client_handler(const MessagePtr& msg, void* args)
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
    node_->join(srv_name_);
    node_->async_spin();
    DLOG(INFO) << "waiting for server" << std::endl;
    node_->wait_join(srv_name_); // wait for service
    DLOG(INFO) << "server joined group" << std::endl;
    node_->register_whispers(&client_handler, (void*)this);
  }

  Client::~Client()
  {
    node_->leave(srv_name_);
    node_->stop();
    delete node_;
  }
  
  std::string
  Client::call_srv(const std::string& req, int timeout_s)
  {
    message_received_.store(false);
    response_ = std::string("");
    node_->shout(srv_name_, req);
    double start_t = clock();

    { 
      // wait for the response
      std::unique_lock<std::mutex> lock(response_mutex_);
      response_cond_.wait_for(lock, std::chrono::seconds(timeout_s), 
			      [&]{ return message_received_.load(); });
    }
    
    if (!message_received_.load()) throw ServiceCallTimeout();

    return response_;
  }

  void 
  Client::handle_response(const MessagePtr& resp)
  {
    DLOG(INFO) << "received response\n" << resp->msg << std::endl;
    response_ = resp->msg;
    message_received_.store(true);
    response_cond_.notify_one();
  }

  void 
  Client::spin()
  {
    node_->spin();
  }

  ServiceReplyPtr
  Client::call(const std::string& msg)
  {
    return boost::make_shared<ServiceReply>();
  }

}
