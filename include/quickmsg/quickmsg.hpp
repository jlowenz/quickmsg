#pragma once

#include <quickmsg/subscriber.hpp>
#include <quickmsg/publisher.hpp>
#include <quickmsg/service.hpp>
#include <quickmsg/client.hpp>

namespace quickmsg {
  
  /**
   * \brief Initialize the quickmsg system. 
   * Must be called once per process.
   */
  void init(const std::string& name);
  void shutdown(const std::string& reason = "");

}
