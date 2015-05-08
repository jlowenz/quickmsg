#include <quickmsg/service.hpp>
#include <quickmsg/service_wrap.h>

using namespace quickmsg;

extern "C" {

qm_service_t *
qm_service_new (const char* srv_name,
                 const char* (*impl)(const Message*)) 
{
  std::cout<<" Creating service "<<srv_name<<std::endl;
  Service* service = new Service(srv_name, impl);
  return reinterpret_cast<qm_service_t*>(service);
} 

void 
qm_service_destroy(qm_service_t *self_p)
{
  Service* service = reinterpret_cast<Service*>(self_p);
  delete service;
}

void 
qm_service_spin(qm_service_t *self_p)
{
  Service* service = reinterpret_cast<Service*>(self_p);
  service->spin();
}

}
