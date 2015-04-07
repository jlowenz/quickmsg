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

    std::string call_srv(const std::string& req, double timeout_s=10.0);
    void handle_response(const MessagePtr& resp);
    bool interrupted();
    void spin();

  private:
    std::string srv_name_;
    std::string response_;
    GroupNode* node_;
    bool wait_for_response_;
  };

}
