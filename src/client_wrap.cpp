#include <quickmsg/client.hpp>
#include <quickmsg/client_wrap.h>

using namespace quickmsg;

extern "C" {

qmg_client_t *
qmg_client_new (const char* srv_name) 
{
  std::cout<<" Creating client to service "<<srv_name<<std::endl;
  Client* client = new Client(srv_name);
  return reinterpret_cast<qmg_client_t*>(client);
} 

void 
qmg_client_destroy(qmg_client_t *self_p)
{
  Client* client = reinterpret_cast<Client*>(self_p);
  delete client;
}

const char* 
qmg_call_srv(qmg_client_t *self_p, const char* req)
{
  Client* client = reinterpret_cast<Client*>(self_p);
  return client->call_srv(req).c_str();
}
}
