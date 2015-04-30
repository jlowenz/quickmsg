#include <quickmsg/publisher.hpp>
#include <quickmsg/publisher_wrap.h>

using namespace quickmsg;

extern "C" {

qmg_publisher_t *
qmg_publisher_new (const char* topic) 
{
  std::cout<<" Creating publisher with topic "<<topic<<std::endl;
  Publisher* pub = new Publisher(topic);
  return reinterpret_cast<qmg_publisher_t*>(pub);
} 

void 
qmg_publisher_destroy(qmg_publisher_t *self_p)
{
  Publisher* pub = reinterpret_cast<Publisher*>(self_p);
  delete pub;
}

void 
qmg_publish(qmg_publisher_t *self_p, const char* msg)
{
  Publisher* pub = reinterpret_cast<Publisher*>(self_p);
  pub->publish(msg);
}
}
