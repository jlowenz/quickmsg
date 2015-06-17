#pragma once

#include <tbb/concurrent_queue.h>
#include <type_traits>
#include <quickmsg/types.hpp>
#include <quickmsg/group_node.hpp>

namespace quickmsg {

	class Service
  {
    friend void service_handler(const Message*, void*);
  public:
    Service(const std::string& srv_name,
            size_t queue_size=10);

    Service(const std::string& srv_name,
            ServiceCallback cb,
	    void* args=NULL,
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

  protected:
    /** The service handler implementation processes the incoming
	message and passes the result to the
	service_implementation. This method should generally not be
	overridden.
    */
    virtual void handle_request(const Message* req);  
  private:
    std::string srv_name_;
    std::string promisc_topic_;
    ServiceCallback impl_;
    void* args_;
    GroupNode* node_;
    tbb::concurrent_bounded_queue<MessagePtr> reqs_;

    void init(size_t queue_size);    
  };
  
}
