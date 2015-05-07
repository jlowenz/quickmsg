#include <quickmsg/subscriber.hpp>
#include <type_traits>

namespace quickmsg {

  void
  default_cb(const Message* msg)
  {
    std::cout << " Default subscriber impl (echo) " << std::endl;
    std::cout << (*msg) << std::endl;
  }

  /**
   * \internal 
   */
  void subscriber_handler(const MessagePtr& msg, void* args)
  {
    static_cast<Subscriber*>(args)->handle_message(msg);
  }

  void async_subscriber_handler(const MessagePtr& msg, void* args)
  {
    static_cast<AsyncSubscriber*>(args)->handle_message(msg);
  }

  Subscriber::Subscriber(const std::string& topic, const SubscriberImpl& impl,
                         size_t queue_size)
    : topic_(topic), impl_(impl)
  {
    init(queue_size);
  }

  Subscriber::Subscriber(const std::string& topic, size_t queue_size)
    : topic_(topic)
  {
    impl_=default_cb;
    init(queue_size);
  }

  void Subscriber::init(size_t queue_size)
  {
    // set the size of our bounded queue
    msgs_.set_capacity(queue_size);
    // create the group node
    std::string name("S/");
    node_ = new GroupNode(name + topic_);
    // join the correct group (topic)
    node_->join(topic_);
    // register the message handler for the group
    node_->register_handler(topic_, &subscriber_handler, (void*)this);
    // start spinning asynchronously, returns immediately
    std::cout<<"Subscribing on topic "<<topic_<<std::endl;
    node_->async_spin();
  }

  Subscriber::~Subscriber()
  {
    node_->leave(topic_);
    node_->stop();
    delete node_;
  }

  bool Subscriber::interrupted()
  {
    return node_->interrupted();
  }

  void Subscriber::join()
  {
    node_->join();
  }
    
  void Subscriber::handle_message(const MessagePtr& msg)
  {
    // if the queue is full, too bad!
    std::cout << "Received message "<< msg->msg<<std::endl;
    subscriber_impl(msg.get());
    msgs_.try_push(msg);
  }

  void Subscriber::subscriber_impl(const Message* msg)
  {
    impl_(msg);
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

  AsyncSubscriber::AsyncSubscriber(const std::string& topic, const SubscriberImpl& impl)
    : topic_(topic), impl_(impl)
  {
    init();
  }

  AsyncSubscriber::AsyncSubscriber(const std::string& topic)
    : topic_(topic)
  {
    impl_=default_cb;
    init();
  }

  void AsyncSubscriber::init()
  {
    // create the group node
    std::string name("AS/");
    node_ = new GroupNode(name + topic_);
    // join the correct group (topic)
    node_->join(topic_);
    // register the message handler for the group
    std::cout<<"Async Subscribing on topic "<<topic_<<std::endl;
    node_->register_handler(topic_, &async_subscriber_handler, (void*)this);
  }
  AsyncSubscriber::~AsyncSubscriber()
  {
    node_->leave(topic_);
    node_->stop();
    delete node_;
  }

  void AsyncSubscriber::handle_message(const MessagePtr& msg)
  {
    std::cout << "Received message "<< msg->msg<<std::endl;
    subscriber_impl(msg.get());
  }

  void AsyncSubscriber::subscriber_impl(const Message* msg)
  {
    impl_(msg);
  }

  bool AsyncSubscriber::interrupted()
  {
    return node_->interrupted();
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
