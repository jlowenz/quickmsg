#include <quickmsg/quickmsg.hpp>
#include <quickmsg/group_node.hpp>

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

// qmg_message_t *
// qmg_message_new () 
// {
//   std::cout<<" Creating message with topic "<<topic<<std::endl;
//   Message* msg = new Message();
//   return reinterpret_cast<qmg_subscriber_t*>(sub);
// }

// void 
// qmg_message_destroy(qmg_message_t *self_p)
// {
//   Message* msg = reinterpret_cast<Message*>(self_p);
//   delete msg;
// }

}
