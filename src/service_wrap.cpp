#include <quickmsg/service.hpp>
#include <quickmsg/service_wrap.h>

using namespace quickmsg;

extern "C" {

qmg_service_t *
qmg_service_new (const char* srv_name,
                 const char* (*impl)(const char*)) 
{
  std::cout<<" Creating service "<<srv_name<<std::endl;
  Service* service = new Service(srv_name, impl);
  return reinterpret_cast<qmg_service_t*>(service);
} 

void 
qmg_service_destroy(qmg_service_t *self_p)
{
  Service* service = reinterpret_cast<Service*>(self_p);
  delete service;
}

void 
qmg_service_spin(qmg_service_t *self_p)
{
  Service* service = reinterpret_cast<Service*>(self_p);
  service->spin();
}

}
