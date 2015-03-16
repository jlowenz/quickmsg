#pragma once

#include <quickmsg/types.hpp>
#include <quickmsg/group_node.hpp>

namespace quickmsg {

  class Client
  {
  public:
    Client(const std::string& channel);
    virtual ~Client();
    
    ServiceReplyPtr call(const std::string& msg);
  };

}
