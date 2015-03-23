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
  void init(std::string& name);
  void shutdown(std::string& reason = "");

}
