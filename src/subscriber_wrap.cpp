#include <quickmsg/subscriber.hpp>
#include <quickmsg/subscriber_wrap.h>
#include "cqm_util.hpp"

using namespace quickmsg;

typedef struct _c_handler
{
  qm_message_handler_t handler;
  void* args;
} c_handler_t;

struct _qm_async_subscriber_t
{
  AsyncSubscriber* self;
  c_handler_t* handler;
};

static inline Subscriber* from(qm_subscriber_t s)
{
  return reinterpret_cast<Subscriber*>(s);
}
  
static inline qm_subscriber_t to(Subscriber* s)
{
  return reinterpret_cast<qm_subscriber_t>(s);
}


static inline qm_vec_qm_message_t to(MsgList* m)
{
  return reinterpret_cast<qm_vec_qm_message_t>(m);
}


static inline AsyncSubscriber* from(qm_async_subscriber_t s)
{
  return s->self;
}

extern "C" {

  qm_subscriber_t
  qm_subscriber_new (const char* topic, int queue_sz) 
  {
    Subscriber* sub = new Subscriber(topic, queue_sz);
    return to(sub);
  } 

  qm_vec_qm_message_t
  qm_subscriber_get_messages(qm_subscriber_t self_p)
  {
    Subscriber* sub = from(self_p);
    MsgList msgs = sub->messages(); // copies
    MsgList* c_msgs = new MsgList(msgs); // copies AGAIN :-(
    return to(c_msgs);
  }

  void
  qm_subscriber_destroy(qm_subscriber_t self_p)
  {
    Subscriber* sub = reinterpret_cast<Subscriber*>(self_p);
    delete sub;
  }
  
  void _async_message_handler(const Message* msg, void* args)
  {
    //qm_message_t m = to(msg);
    c_handler_t* data = static_cast<c_handler_t*>(args);
    data->handler(to(msg), data->args);
  }

  qm_async_subscriber_t
  qm_async_subscriber_new(const char* topic, qm_message_handler_t handler, void* args)
  {    
    c_handler_t* h_data = new c_handler_t;
    h_data->handler = handler;
    h_data->args = args;
    AsyncSubscriber* asub = new AsyncSubscriber(topic, _async_message_handler, (void*)h_data);
    qm_async_subscriber_t self_p = new struct _qm_async_subscriber_t;
    self_p->self = asub;
    self_p->handler = h_data;
    return self_p;
  }
  
  void
  qm_async_subscriber_spin(qm_async_subscriber_t self)
  {
    self->self->spin();
  }

  void
  qm_async_subscriber_destroy(qm_async_subscriber_t self_p)
  {
    delete self_p->self;
    delete self_p->handler;
    delete self_p;
  }

}
