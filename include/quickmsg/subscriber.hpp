#pragma once

#include <tbb/concurrent_queue.h>
#include <quickmsg/types.hpp>
#include <quickmsg/group_node.hpp>
#include <boost/shared_ptr.hpp>
#include <list>

namespace quickmsg {
  
  
  class Subscriber
  {
  public:    
    Subscriber(const std::string& topic, size_t queue_size = 10);
    virtual ~Subscriber();
    
    /** \brief Return messages that have arrived since the last call.
	
	\return a list of messages
     */
    MsgListPtr messages();
    
  private:
    /**
     * \internal C wrapper function to call the subscribers message handler
     */
    friend void subscriber_handler(const MessagePtr& msg, void* args); 

    void handle_message(const MessagePtr& msg);

    std::string topic_;
    GroupNode* node_;
    tbb::concurrent_bounded_queue<MessagePtr> msgs_;
  };

  class AsyncSubscriber
  {
  public:
    AsyncSubscriber(const std::string& topic, MessageCallback cb, void* args=NULL);
    virtual ~AsyncSubscriber();
        
    void spin();
    void async_spin();
  private:
    void handle_message(const MessagePtr& msg);
    std::string topic_;
    GroupNode* node_;
  };  
}
