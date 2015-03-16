#pragma once

#include <type_traits>
#include <quickmsg/types.hpp>
#include <quickmsg/group_node.hpp>

namespace quickmsg {

  std::string _service_impl(const boost::shared_ptr<Message> request);

  typedef std::add_pointer<decltype(_service_impl)> ServiceImpl;

  class Service
  {
  public:
    Service(const std::string& channel, ServiceImpl impl);
    virtual ~Service();
    
    void spin();
    void async_spin();
  };
  
}
