#include <boost/log/trivial.hpp>
#include <quickmsg/service.hpp>
#include <type_traits>
#include <string.h>

namespace quickmsg {

  void service_handler(const Message* msg, void* args)
  {
    static_cast<Service*>(args)->handle_request(msg);
  }

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

  Service::Service(const std::string& srv_name, ServiceCallback impl,
		   void* args, size_t queue_size)
    : srv_name_(srv_name), impl_(impl), args_(args)
  {
    init(queue_size);
  }

  Service::Service(const std::string& srv_name,
                   size_t queue_size)
    : srv_name_(srv_name)
  {
    impl_ = default_echo;
    init(queue_size);
  }

  void Service::init(size_t queue_size)
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

  Service::~Service()
  {
    node_->leave(srv_name_);
    node_->stop();
    delete node_;
  }

  void 
  Service::handle_request(const Message* req)
  {
    // if the queue is full, too bad!
    //MessagePtr msg(new Message(*req));
    //reqs_.try_push(msg);
    
    BOOST_LOG_TRIVIAL(debug) << "got request: " << req->msg << std::endl;
    std::string resp_str(service_impl(req));
    BOOST_LOG_TRIVIAL(debug) << "add request\n" << req->msg << "response\n" << resp_str << std::endl;
    // whisper the response back
    node_->whisper(req->header.src_uuid, resp_str);
    // TODO: are we going to should the response to the promiscuous group too?
  }

  std::string 
  Service::service_impl(const Message* req)
  {
    char* resp_chars = impl_(req, args_); // Don't leak mem
    std::string resp(resp_chars);
    free(resp_chars);
    return resp;
  }

  void 
  Service::spin()
  {
    node_->spin();
  }

  void 
  Service::async_spin()
  {
    node_->async_spin();
  }

}
