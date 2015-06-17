#pragma once

#include <quickmsg/types.hpp>
#include <quickmsg/group_node.hpp>

namespace quickmsg {

	class Publisher
  {
  public:
    Publisher(const std::string& topic);
    virtual ~Publisher();

    void publish(const std::string& msg);
    void join();
  private:
    std::string topic_;
    GroupNode* node_;
  };

}
