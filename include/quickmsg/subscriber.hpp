#pragma once

#include <tbb/concurrent_queue.h>
#include <quickmsg/types.hpp>
#include <quickmsg/group_node.hpp>
#include <boost/shared_ptr.hpp>
#include <list>

namespace quickmsg {

  //  void default_cb(const char* msg);
  void default_cb(const Message* msg);

  /**
   * A Subscriber instance will register on the provided topic and
   * listen for messages in a separate thread, delivering them to a
   * queue for later *synchronous* retrieval. The messages may be
   * retrieved using the messages() member. 
   */
  class Subscriber
  {
  public:    

    /** 
     * Construct a new subscriber on the given topic with the
     * specified queue size. If the messages are not retrieved before
     * the queue size is exceeded, then messages will be dropped.
     */
    Subscriber(const std::string& topic, size_t queue_size=10);
    virtual ~Subscriber();
    
    /**
     * Retrieve the messages currently on the queue. An empty list
     * will be returned if there are no messages. Otherwise, the list
     * will contain Message* that the caller owns (and is responsible
     * for deleting).
     */
    MsgList messages();

    void join();
  protected:
    virtual void handle_message(const Message* msg);
  private:
    friend void subscriber_handler(const Message* msg, void* args);
    std::string topic_;
    MessageCallback impl_;
    void* args_;
    GroupNode* node_;
    tbb::concurrent_bounded_queue<Message*> msgs_;
    void init(size_t queue_size);
  };

  /**
   * An AsyncSubscriber registers on the provided topic and listens
   * for messages and will deliver them asynchronously to the provided
   * MessageCallback. Alternatively, a subclass can override the
   * handle_message() method in order to receive the messages as they
   * arrive. Use the spin() method to block the caller, or the
   * async_spin() method to start up another thread and allow the
   * caller to continue.
   */
  class AsyncSubscriber
  {
  public:
    AsyncSubscriber(const std::string& topic, MessageCallback impl, void* args=NULL);
    AsyncSubscriber(const std::string& topic);
    virtual ~AsyncSubscriber();

    void spin();
    void async_spin();

    virtual void handle_message(const Message* msg);
  private:
    friend void async_subscriber_handler(const Message*,void*);
    std::string topic_;
    MessageCallback impl_;
    void* args_;
    GroupNode* node_;
    void init();
  };  
}
