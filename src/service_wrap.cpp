#include <quickmsg/service.hpp>
#include <quickmsg/service_wrap.h>
#include "cqm_util.hpp"

using namespace quickmsg;

typedef struct _c_handler
{
  qm_service_handler_t handler;
  void* args;
} c_handler_t;

struct _qm_service_t
{
  Service* self;
  c_handler_t* handler;
};

static inline Service* from(qm_service_t s)
{
  return reinterpret_cast<Service*>(s);
}

extern "C" {

  
  char* _service_handler(const Message* msg, void* arg)
  {
    qm_message_t m = to(msg);
    c_handler_t* data = static_cast<c_handler_t*>(arg);
    return data->handler(m, data->args);
  }

  qm_service_t
  qm_service_new (const char* srv_name, qm_service_handler_t impl, void* arg) 
  {
    c_handler_t* h_data = new c_handler_t;
    h_data->handler = impl;
    h_data->args = arg;
    qm_service_t svc = new struct _qm_service_t;
    svc->self = new Service(srv_name, _service_handler, (void*)h_data);
    svc->handler = h_data;
    return svc;
  } 

  void 
  qm_service_destroy(qm_service_t self_p)
  {
    delete self_p->self;
    delete self_p->handler;
    delete self_p;
  }

  void 
  qm_service_spin(qm_service_t self_p)
  {
    if (!self_p) return;
    self_p->self->spin(); // async spin too??
  }

}
