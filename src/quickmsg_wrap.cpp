#include <quickmsg/quickmsg.hpp>
#include <quickmsg/group_node.hpp>
#include <quickmsg/quickmsg_wrap.h>

using namespace quickmsg;

extern "C" {

  void
  qm_init(const char* name) 
  {
    std::cout << "qm_init " << name << std::endl;
    std::string sname(name);
    init(sname);
  }

  void
  qm_shutdown(const char* reason)
  {
    shutdown(reason);
  }

  bool
  qm_ok()
  {
    return ok();
  }

  double
  qm_get_msg_stamp(qm_message_t *self_p)
  {
    Message* msg = reinterpret_cast<Message*>(self_p);
    return msg->get_stamp();
  }

  const char* 
  qm_get_msg_src(qm_message_t *self_p)
  {
    Message* msg = reinterpret_cast<Message*>(self_p);
    return msg->get_src().c_str();
  }

  const char* 
  qm_get_msg_str(qm_message_t *self_p)
  {
    Message* msg = reinterpret_cast<Message*>(self_p);
    return msg->get_msg().c_str();
  }

  bool
  qm_get_successful(qm_service_reply_t* self_p)
  {
    ServiceReply* rep = reinterpret_cast<ServiceReply*>(self_p);
    return rep->successful;
  }

}
