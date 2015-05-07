#pragma once

#include <tbb/concurrent_queue.h>
#include <type_traits>
#include <quickmsg/types.hpp>
#include <quickmsg/group_node.hpp>

namespace quickmsg {

  //  std::string _service_impl(const MessagePtr& request);

  //  std::string default_echo(const std::string& req);

  const char* default_echo(const Message* req);

  typedef std::add_pointer<decltype(default_echo)>::type ServiceImpl;

  class Service
  {
  public:
    Service(const std::string& srv_name,
            const ServiceImpl& impl,
            const std::string& promisc_topic=std::string("promisc"),
            size_t queue_size=10);
    Service(const std::string& srv_name,
            const std::string& promisc_topic=std::string("promisc"),
            size_t queue_size=10);
    void init(size_t queue_size);
    virtual ~Service();
    
    virtual void handle_request(const MessagePtr& req);
    virtual std::string service_impl(const Message* req);
    void publish(const std::string& msg);
    void respond(const PeerPtr& peer, const std::string& resp);
    bool interrupted();
    void spin();
    void async_spin();

  private:
    std::string srv_name_;
    std::string promisc_topic_;
    ServiceImpl impl_;
    GroupNode* node_;
    tbb::concurrent_bounded_queue<MessagePtr> reqs_;
  };
  
}
