#include <quickmsg/client.hpp>

namespace quickmsg {

  Client::Client(const std::string& channel)
  {
  }

  Client::~Client()
  {
  }
  
  ServiceReplyPtr
  Client::call(const std::string& msg)
  {
    return boost::make_shared<ServiceReply>();
  }

}
