#pragma once

#include <quickmsg/types.hpp>
#include <quickmsg/group_node.hpp>

namespace quickmsg {

  class Client
  {
  public:
    Client(const std::string& srv_name);
    virtual ~Client();
    
    ServiceReplyPtr call(const std::string& msg);

    void call_srv(const std::string& req, double timeout_s=10.0);
    void handle_response(const MessagePtr& resp);
    bool interrupted();
    void spin();

  private:
    std::string srv_name_;
    GroupNode* node_;
    std::atomic_bool wait_for_response_;
  };

}
