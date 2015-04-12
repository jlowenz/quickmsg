#include <quickmsg/service.hpp>
#include <quickmsg/add_n_ints.hpp>
#include <type_traits>

namespace quickmsg {

  void service_handler(const MessagePtr& msg, void* args)
  {
    static_cast<Service*>(args)->handle_request(msg);
  }

  Service::Service(const std::string& srv_name,
                   const std::string& promisc_topic, size_t queue_size)
    : promisc_topic_(promisc_topic), srv_name_(srv_name)
  {
    reqs_.set_capacity(queue_size);
    std::string name("Srv/");
    node_ = new GroupNode(name + srv_name);
    node_->join(srv_name);
    node_->join(promisc_topic_);

    // Server will sub on svc_topic, whisper to client on svc_topic, pub on prom_topic
    node_->register_handler(srv_name_, &service_handler, (void*)this);
    //    node_->register_whispers(&service_handler, (void*)this);
    //    node_->async_spin();
  }

  Service::~Service()
  {
    node_->leave(promisc_topic_);
    node_->leave(srv_name_);
    node_->stop();
    delete node_;
  }

  void 
  Service::handle_request(const MessagePtr& req)
  {
    // if the queue is full, too bad!
    reqs_.try_push(req);

    std::string resp_str = service_impl(req->msg);
    std::cout << "add request\n" << req->msg << "response\n" << resp_str << std::endl;
    node_->whisper(req->header.src_uuid, resp_str);
  }

  std::string 
  Service::service_impl(const std::string &req)
  {
    std::cout << " Default service impl (echo request) " << std::endl;
    return req;
  }

  void 
  Service::publish(const std::string& msg)
  {
    //    node_->shout(promisc_topic_, msg);
  }

  // Make response message
  void 
  Service::respond(const PeerPtr& peer, const std::string& resp)
  {
    //    node_->whisper(peer, resp);
  }

  bool
  Service::interrupted()
  {
    return node_->interrupted();
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
