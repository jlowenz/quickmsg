#include <quickmsg/quickmsg.hpp>
#include <quickmsg/group_node.hpp>
#include <quickmsg/quickmsg_wrap.h>

using namespace quickmsg;

extern "C" {

void
qmg_init(const std::string& name) 
{
  init(name);
}

void
qmg_shutdown(const std::string& reason)
{
  shutdown(reason);
}

double
qmg_message_get_stamp(qmg_message_t *self_p)
{
  Message* msg = reinterpret_cast<Message*>(self_p);
  return msg->get_stamp();
}

const char* 
qmg_message_get_src(qmg_message_t *self_p)
{
  Message* msg = reinterpret_cast<Message*>(self_p);
  return msg->get_src().c_str();
}

const char* 
qmg_message_get_msg_str(qmg_message_t *self_p)
{
  Message* msg = reinterpret_cast<Message*>(self_p);
  return msg->get_msg().c_str();
}

}
