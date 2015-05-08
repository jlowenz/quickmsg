#pragma once

#include <tbb/concurrent_queue.h>
#include <quickmsg/types.hpp>
#include <quickmsg/group_node.hpp>
#include <boost/shared_ptr.hpp>
#include <list>

namespace quickmsg {

  //  void default_cb(const char* msg);

  void default_cb(const Message* msg);

  typedef std::add_pointer<decltype(default_cb)>::type SubscriberImpl;  
  
  class Subscriber
  {
  public:    
    Subscriber(const std::string& topic, const SubscriberImpl& impl, size_t queue_size=10);
    Subscriber(const std::string& topic, size_t queue_size=10);
    virtual void init(size_t queue_size);
    virtual ~Subscriber();
    
    virtual void handle_message(const MessagePtr& msg);
    virtual void subscriber_impl(const Message* msg);
    MsgListPtr messages();
    void join();
  private:
    friend void subscriber_handler(const MessagePtr& msg, void* args);
    SubscriberImpl impl_;
    std::string topic_;
    GroupNode* node_;
    tbb::concurrent_bounded_queue<MessagePtr> msgs_;
  };

  class AsyncSubscriber
  {
  public:
    //    AsyncSubscriber(const std::string& topic, MessageCallback cb, void* args=NULL);
    AsyncSubscriber(const std::string& topic, const SubscriberImpl& impl);
    AsyncSubscriber(const std::string& topic);
    void init();
    virtual ~AsyncSubscriber();
    void spin();
    void async_spin();
    virtual void handle_message(const MessagePtr& msg);
    virtual void subscriber_impl(const Message* msg);
  private:
    SubscriberImpl impl_;
    std::string topic_;
    GroupNode* node_;
  };  
}
