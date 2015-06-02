#include <quickmsg/client.hpp>
#include <quickmsg/client_wrap.h>

using namespace quickmsg;

extern "C" {

  qm_client_t *
  qm_client_new (const char* srv_name) 
  {
    std::cout<<" Creating client to service "<<srv_name<<std::endl;
    Client* client = new Client(srv_name);
    return reinterpret_cast<qm_client_t*>(client);
  } 

  void 
  qm_client_destroy(qm_client_t *self_p)
  {
    Client* client = reinterpret_cast<Client*>(self_p);
    delete client;
  }

  const char* 
  qm_call_srv(qm_client_t *self_p, const char* req)
  {
    Client* client = reinterpret_cast<Client*>(self_p);
    return client->calls(req).c_str();
  }
}
