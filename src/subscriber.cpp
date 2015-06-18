//#include <glog/logging.h>
#include <boost/log/trivial.hpp>
#include <quickmsg/subscriber.hpp>
#include <type_traits>

namespace quickmsg {

  void
  default_cb(const Message* msg, void*)
  {
    BOOST_LOG_TRIVIAL(debug) << " Default subscriber impl (echo) " << std::endl;
    std::cout << "Default Callback: [probably not what you want]\n" << (*msg) << std::endl;
  }

  /**
   * \internal 
   */
  void subscriber_handler(const Message* msg, void* args)
  {
    static_cast<Subscriber*>(args)->handle_message(msg);
  }

  void async_subscriber_handler(const Message* msg, void* args)
  {
    static_cast<AsyncSubscriber*>(args)->handle_message(msg);
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
    BOOST_LOG_TRIVIAL(info) << "Subscribing on topic "<<topic_<<std::endl;
    node_->async_spin();
  }

  Subscriber::~Subscriber()
  {
    node_->leave(topic_);
    node_->stop();
    delete node_;
  }

  void Subscriber::join()
  {
    node_->join();
  }
    
  void Subscriber::handle_message(const Message* msg)
  {
    // if the queue is full, too bad!
    BOOST_LOG_TRIVIAL(debug) << "Received message "<< msg->msg<<std::endl;
    msgs_.try_push(new Message(*msg));
  }


  /** \brief Return messages that have arrived since the last call.
      
      \return a list of messages
  */
  MsgList
  Subscriber::messages()
  {
    MsgList mlist;
    Message* m;
    while (msgs_.try_pop(m)) { // terminates when there are no more elements in the queue
      mlist.push_back(m);
    }
    return mlist;
  }

  AsyncSubscriber::AsyncSubscriber(const std::string& topic, 
				   MessageCallback impl, 
				   void* args)
    : topic_(topic), impl_(impl), args_(args)
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
    BOOST_LOG_TRIVIAL(info) <<"Async Subscribing on topic "<<topic_<<std::endl;
    node_->register_handler(topic_, &async_subscriber_handler, (void*)this);
  }

  AsyncSubscriber::~AsyncSubscriber()
  {
    node_->leave(topic_);
    node_->stop();
    delete node_;
  }

  void AsyncSubscriber::handle_message(const Message* msg)
  {
    BOOST_LOG_TRIVIAL(debug) << "Received message "<< msg->msg <<std::endl;
    impl_(msg, args_);
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
