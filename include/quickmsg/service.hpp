#pragma once

#include <type_traits>
#include <quickmsg/types.hpp>

namespace quickmsg {

  class ServiceImpl;

  class Service
  {
  public:
    Service(const std::string& srv_name,
            size_t queue_size=10);

    Service(const std::string& srv_name,
            ServiceCallback cb,
	    void* args,
            size_t queue_size=10);
    virtual ~Service();
    
    /**
     * Override this method to implement how the service responds to
     * the request. By default, this method will call the
     * ServiceCallback given in the constructor, or will call the
     * default "echo" service implementation, which simply returns the
     * request as the reply.
     */
    virtual std::string service_impl(const Message* req);

    /** Block the calling thread while the Service processes incoming
	messages.
     */
    void spin();

    /** Start a thread to sping for this Service, allowing the calling
	thread to continue.  The implication here is that now there
	are multiple threads running, and the handler must be
	thread-aware.
     */
    void async_spin();

  private:
    ServiceImpl* self;
  };
  
}
