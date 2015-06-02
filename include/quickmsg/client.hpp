#pragma once

#include <quickmsg/types.hpp>
#include <quickmsg/group_node.hpp>
#include <exception>
#include <atomic>
#include <mutex>
#include <condition_variable>

namespace quickmsg {

  struct ServiceCallTimeout : public std::runtime_error {
    ServiceCallTimeout() : std::runtime_error("ServiceCallTimeout") {}
  };

  class Client
  {
    friend void client_handler(const Message*,void*);
  public:
    Client(const std::string& srv_name);
    virtual ~Client();
    
    ServiceReplyPtr call(const std::string& msg, int timeout_s=10);
    std::string calls(const std::string& req, int timeout_s=10);

  protected:
    virtual void handle_response(const Message* resp);
  private:
    std::string srv_name_;
    ServiceReplyPtr response_;
    GroupNode* node_;
    std::atomic_bool message_received_;
    std::mutex response_mutex_;
    std::condition_variable response_cond_;
  };

}
