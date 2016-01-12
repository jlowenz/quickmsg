#pragma once

#include <quickmsg/types.hpp>
#include <exception>
#include <stdexcept>
#include <atomic>
#include <mutex>
#include <condition_variable>

namespace quickmsg {

  struct ServiceCallTimeout : public std::runtime_error {
    ServiceCallTimeout() : std::runtime_error("ServiceCallTimeout") {}
    ServiceCallTimeout(const std::string& msg) : std::runtime_error(std::string("ServiceCallTimeout") + msg) {}
    virtual ~ServiceCallTimeout() {}
#if _WIN32
    const char* what() const { return std::runtime_error::what(); }
#else
    const char* what() const noexcept{ return std::runtime_error::what(); }
#endif // _WIN32
  };

  
  class GroupNode;
  class Client
  {
    friend void client_handler(const Message*,void*);
  public:
    Client(const std::string& srv_name);
    virtual ~Client();
    
#if _WIN32
    ServiceReplyPtr call(const std::string& msg, int timeout_s = 10);
    std::string calls(const std::string& req, int timeout_s = 10);
#else
    ServiceReplyPtr call(const std::string& msg, int timeout_s=10) throw(ServiceCallTimeout);
    std::string calls(const std::string& req, int timeout_s=10) throw(ServiceCallTimeout);
#endif

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
