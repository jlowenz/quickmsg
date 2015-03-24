#include <quickmsg/subscriber.hpp>
#include <type_traits>

namespace quickmsg {

  /**
   * \internal 
   */
  void subscriber_handler(const MessagePtr& msg, void* args)
  {
    static_cast<Subscriber*>(args)->handle_message(msg);
  }

  // should subscriber be a subclass of AsyncSubscriber?
  Subscriber::Subscriber(const std::string& topic, size_t queue_size)
    : topic_(topic)
  {
    // set the size of our bounded queue
    msgs_.set_capacity(queue_size);
    // create the group node
    std::string name("S/");
    node_ = new GroupNode(name + topic);
    // join the correct group (topic)
    node_->join(topic_);
    // register the message handler for the group
    node_->register_handler(topic_, &subscriber_handler, (void*)this);
    // start spinning asynchronously, returns immediately
    node_->async_spin();
  }
  
  Subscriber::~Subscriber()
  {
    node_->leave(topic_);
    node_->stop();
    delete node_;
  }
    
  void Subscriber::handle_message(const MessagePtr& msg)
  {
    // if the queue is full, too bad!
    msgs_.try_push(msg);
  }

  /** \brief Return messages that have arrived since the last call.
      
      \return a list of messages
  */
  MsgListPtr
  Subscriber::messages()
  {
    MsgListPtr mlist = boost::make_shared<MsgList>();
    MessagePtr m;
    while (msgs_.try_pop(m)) { // terminates when there are no more elements in the queue
      mlist->push_back(m);
    }
    return mlist;
  }

  AsyncSubscriber::AsyncSubscriber(const std::string& topic, MessageCallback cb, void* args)
    : topic_(topic)
  {
    // create the group node
    std::string name("AS/");
    node_ = new GroupNode(name + topic);
    // join the correct group (topic)
    node_->join(topic_);
    // register the message handler for the group
    node_->register_handler(topic_, cb, args);
  }
  AsyncSubscriber::~AsyncSubscriber()
  {
    node_->leave(topic_);
    node_->stop();
    delete node_;
  }
   
  void AsyncSubscriber::spin()
  {
    node_->spin();
  }

  void AsyncSubscriber::async_spin()
  {
    node_->async_spin();
  }

}
