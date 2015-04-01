#pragma once

#include <tbb/concurrent_queue.h>
#include <type_traits>
#include <quickmsg/types.hpp>
#include <quickmsg/group_node.hpp>

namespace quickmsg {

  std::string _service_impl(const MessagePtr& request);

  typedef std::add_pointer<decltype(_service_impl)>::type ServiceImpl;

  class Service
  {
  public:
    Service(const std::string& srv_name, ServiceImpl impl, 
            const std::string& promisc_topic=std::string("promisc"),
            size_t queue_size=10);
    virtual ~Service();
    
    void handle_request(const MessagePtr& req);
    void publish(const std::string& msg);
    void respond(const PeerPtr& peer, const std::string& resp);
    bool interrupted();
    void spin();
    void async_spin();

  private:
    std::string srv_name_;
    std::string promisc_topic_;
    GroupNode* node_;
    ServiceImpl impl_;
    tbb::concurrent_bounded_queue<MessagePtr> reqs_;
  };
  
}
