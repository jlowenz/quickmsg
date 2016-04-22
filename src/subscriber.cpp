//#include <glog/logging.h>
#include <boost/log/trivial.hpp>
#include <quickmsg/subscriber.hpp>
#include <quickmsg/group_node.hpp>
#include <tbb/concurrent_queue.h>
#include <type_traits>
#include <iostream>

#ifdef SWIGPYTHON
#include <Python.h>
#endif

namespace quickmsg {

  extern bool ok();

  void
  default_cb(const Message* msg, void*)
  {
    BOOST_LOG_TRIVIAL(debug) << " Default subscriber impl (echo) " << std::endl;
    std::cout << "Default Callback: [probably not what you want]\n" << (*msg) << std::endl;
  }


  class SubscriberImpl
  {
  public:    

    /** 
     * Construct a new subscriber on the given topic with the
     * specified queue size. If the messages are not retrieved before
     * the queue size is exceeded, then messages will be dropped.
     */
    SubscriberImpl(const std::string& topic, size_t queue_size=10);
    virtual ~SubscriberImpl();
    
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
   * \internal 
   */
  void subscriber_handler(const Message* msg, void* args)
  {
    static_cast<SubscriberImpl*>(args)->handle_message(msg);
  }


  SubscriberImpl::SubscriberImpl(const std::string& topic, size_t queue_size)
    : topic_(topic)
  {
    assert(ok() && "quickmsg shutdown or quickmsg::init() must be called first");
    impl_=default_cb;
    init(queue_size);
  }
  Subscriber::Subscriber(const std::string& topic, size_t queue_size)
    : self(new SubscriberImpl(topic, queue_size))
  {    
  }

  void SubscriberImpl::init(size_t queue_size)
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

  SubscriberImpl::~SubscriberImpl()
  {
    node_->leave(topic_);
    //node_->stop();
    delete node_;
  }
  Subscriber::~Subscriber()
  {
    delete self;
  }

  void SubscriberImpl::join()
  {
    node_->join();
  }
  void Subscriber::join()
  {
    self->join();
  }
    
  void SubscriberImpl::handle_message(const Message* msg)
  {
    // if the queue is full, too bad!
    BOOST_LOG_TRIVIAL(debug) << "Received message "<< msg->msg<<std::endl;
    msgs_.try_push(new Message(*msg));
  }

  MsgList
  SubscriberImpl::messages()
  {
    MsgList mlist;
    Message* m;
    while (msgs_.try_pop(m)) { // terminates when there are no more elements in the queue
      mlist.push_back(m);
    }
    return mlist;
  }
  MsgList
  Subscriber::messages()
  {
    return self->messages();
  }

#ifdef SWIGPYTHON
  class SWIG_Python_Thread_Block {
    bool status;
    PyGILState_STATE state;
  public:
    void end() { if (status) { PyGILState_Release(state); status = false;} }
    SWIG_Python_Thread_Block() : status(true), state(PyGILState_Ensure()) {}
    ~SWIG_Python_Thread_Block() { end(); }
  };
#endif

  void async_subscriber_handler(const Message* msg, void* args)
  {
#ifdef SWIGPYTHON
    SWIG_Python_Thread_Block ptb;
#endif
    static_cast<AsyncSubscriber*>(args)->handle_message(msg);
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
    //node_->stop();
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
