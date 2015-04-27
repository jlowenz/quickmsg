#include <quickmsg/publisher.hpp>
#include <quickmsg/publisher_wrap.h>

using namespace quickmsg;

extern "C" {

int 
fact(int n) 
{
  return n*10;
}

qmg_publisher_t *
qmg_publisher_new (const std::string& topic) 
{
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
qmg_publish(qmg_publisher_t *self_p, const std::string& msg)
{
  Publisher* pub = reinterpret_cast<Publisher*>(self_p);
  pub->publish(msg);
}
}
