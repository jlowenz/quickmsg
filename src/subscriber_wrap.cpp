#include <quickmsg/subscriber.hpp>
#include <quickmsg/subscriber_wrap.h>

using namespace quickmsg;

extern "C" {

qm_subscriber_t *
qm_subscriber_new (const char* topic, int queue_sz) 
{
  std::cout<<" Creating subscriber with topic "<<topic<<std::endl;
  Subscriber* sub = new Subscriber(topic, queue_sz);
  return reinterpret_cast<qm_subscriber_t*>(sub);
} 

void 
qm_subscriber_destroy(qm_subscriber_t *self_p)
{
  Subscriber* sub = reinterpret_cast<Subscriber*>(self_p);
  delete sub;
}
}
