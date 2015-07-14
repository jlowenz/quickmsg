#pragma once

#include <quickmsg/types.hpp>

namespace quickmsg {

  class GroupNode;
  class Publisher
  {
  public:
    /**
     * Construct a new publisher on the given topic. If the publisher
     * should wait for subscribers before returning, set wait to true
     * (default is to NOT wait for subscribers).
     */
    Publisher(const std::string& topic, bool wait=false);
    virtual ~Publisher();

    void publish(const std::string& msg);
    void join();
  private:
    std::string topic_;
    GroupNode* node_;
  };

}
