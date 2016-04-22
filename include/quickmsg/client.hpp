#pragma once

#include <quickmsg/types.hpp>
#include <exception>
#include <stdexcept>
#include <atomic>
#include <mutex>
#include <condition_variable>
#include <string>

const int DEFAULT_TIMEOUT = 30;

namespace quickmsg {

  class ServiceCallTimeout : public std::runtime_error {
  public:
    ServiceCallTimeout() : std::runtime_error("ServiceCallTimeout") {}
    ServiceCallTimeout(const std::string& msg) : std::runtime_error(std::string("ServiceCallTimeout: ") + msg) {}
    virtual ~ServiceCallTimeout() {}
#if _WIN32
    const char* what() const { return std::runtime_error::what(); }
#else
    const char* what() const noexcept{ return std::runtime_error::what(); }
#endif // _WIN32
  };

  class InvalidResponse : public std::runtime_error {
  public:
    InvalidResponse() : std::runtime_error("InvalidResponse") {}
    InvalidResponse(const std::string& msg) : std::runtime_error(std::string("InvalidResponse: ") + msg) {}
    virtual ~InvalidResponse() {}
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
    Client(const std::string& srv_name, bool wait_for_service = true);
    virtual ~Client();
    
    ServiceReplyPtr call(const std::string& msg, int timeout_s = DEFAULT_TIMEOUT);
    std::string calls(const std::string& req, int timeout_s = DEFAULT_TIMEOUT);

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
