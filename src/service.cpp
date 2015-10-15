#include <boost/log/trivial.hpp>
#include <quickmsg/service.hpp>
#include <quickmsg/group_node.hpp>
#include <tbb/concurrent_queue.h>
#include <type_traits>
#include <string.h>

#ifdef SWIGPYTHON
#include <Python.h>
#endif

namespace quickmsg {


  // std::string
  // default_echo(const std::string& req)
  // {
  //   std::cout << " Default service impl (echo request) " << std::endl;
  //   return req;
  // }

  char*
  default_echo(const Message* req, void* args)
  {
    BOOST_LOG_TRIVIAL(debug) << " Default service impl (echo request) " << std::endl;
    char* resp = (char*)malloc(req->msg.length()+1);
    memcpy(resp, req->msg.c_str(), req->msg.length());
    resp[req->msg.length()] = 0;
    return resp;
  }
  
  class ServiceImpl
  {
    friend void service_handler(const Message*, void*);
  public:
    ServiceImpl(Service* owner,
		const std::string& srv_name,
		size_t queue_size=10);
    
    ServiceImpl(Service* owner,
		const std::string& srv_name,
		ServiceCallback cb,
		void* args=NULL,
		size_t queue_size=10);
    virtual ~ServiceImpl();
    
    

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

    /** The service handler implementation processes the incoming
	message and passes the result to the
	service_implementation. This method should generally not be
	overridden.
    */
    virtual void handle_request(const Message* req);  
    
    Service* owner_;
    std::string srv_name_;
    std::string promisc_topic_;
    ServiceCallback impl_;
    void* args_;
    GroupNode* node_;
    tbb::concurrent_bounded_queue<MessagePtr> reqs_;
    
    void init(size_t queue_size);    
  };
  
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
  
  void service_handler(const Message* msg, void* args)
  {
    static_cast<ServiceImpl*>(args)->handle_request(msg);
  }

  ServiceImpl::ServiceImpl(Service* owner,
			   const std::string& srv_name, 
			   ServiceCallback impl,
			   void* args, 
			   size_t queue_size)
    : owner_(owner), srv_name_(srv_name), impl_(impl), args_(args)
  {
    init(queue_size);
  }
  Service::Service(const std::string& srv_name,
		   ServiceCallback cb,
		   void* args,
		   size_t queue_size)
    : self(new ServiceImpl(this, srv_name, cb, args, queue_size))
  {
  }

  ServiceImpl::ServiceImpl(Service* owner,
			   const std::string& srv_name,
			   size_t queue_size)
    : owner_(owner), srv_name_(srv_name)
  {
    impl_ = default_echo;
    init(queue_size);
  }
  Service::Service(const std::string& srv_name,
		   size_t queue_size)
    : self(new ServiceImpl(this, srv_name, queue_size))
  {
  }


  void ServiceImpl::init(size_t queue_size)
  {
    reqs_.set_capacity(queue_size);
    std::string name("Srv/");
    node_ = new GroupNode(name + srv_name_);
    node_->join(srv_name_);

    // Server will sub on svc_topic, whisper to client on svc_topic, pub on prom_topic
    node_->register_handler(srv_name_, &service_handler, (void*)this);
    //    node_->register_whispers(&service_handler, (void*)this);
    //    node_->async_spin();
  }


  ServiceImpl::~ServiceImpl()
  {
    node_->leave(srv_name_);
    node_->stop();
    delete node_;
  }
  Service::~Service()
  {
    delete self;
  }

  void 
  ServiceImpl::handle_request(const Message* req)
  {
    // if the queue is full, too bad!
    //MessagePtr msg(new Message(*req));
    //reqs_.try_push(msg);
#ifdef SWIGPYTHON
    {
      SWIG_Python_Thread_Block ptb;
#endif    
      BOOST_LOG_TRIVIAL(debug) << "got request: " << req->msg << std::endl;
      std::string resp_str(owner_->service_impl(req));
      BOOST_LOG_TRIVIAL(debug) << "add request\n" << req->msg << "response\n" << resp_str << std::endl;
#ifdef SWIGPYTHON
    }
#endif
    // whisper the response back
    node_->whisper(req->header.src_uuid, resp_str);
    // TODO: are we going to should the response to the promiscuous group too?
  }

  std::string 
  Service::service_impl(const Message* req)
  {
    char* resp_chars = self->impl_(req, self->args_);
    std::string resp(resp_chars);
    free(resp_chars);
    return resp;
  }

  void 
  ServiceImpl::spin()
  {
    node_->spin();
  }
  void
  Service::spin()
  {
    self->spin();
  }

  void 
  ServiceImpl::async_spin()
  {
    node_->async_spin();
  }
  void
  Service::async_spin()
  {
    self->async_spin();
  }
}
