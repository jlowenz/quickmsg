#include <quickmsg/quickmsg.hpp>
#include <quickmsg/group_node.hpp>
#include <quickmsg/quickmsg_wrap.h>

using namespace quickmsg;

extern "C" {

void
qm_init(const std::string& name) 
{
  init(name);
}

void
qm_shutdown(const std::string& reason)
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

}
