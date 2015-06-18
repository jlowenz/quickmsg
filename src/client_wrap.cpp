#include <boost/log/trivial.hpp>
#include <quickmsg/client.hpp>
#include <quickmsg/client_wrap.h>
#include <string.h>

using namespace quickmsg;

extern "C" {

  qm_client_t
  qm_client_new (const char* srv_name) 
  {
    BOOST_LOG_TRIVIAL(debug) << " Creating client to service "<<srv_name<<std::endl;
    Client* client = new Client(srv_name);
    return reinterpret_cast<qm_client_t>(client);
  } 

  void 
  qm_client_destroy(qm_client_t self_p)
  {
    Client* client = reinterpret_cast<Client*>(self_p);
    delete client;
  }

  int 
  qm_call_srv(qm_client_t self_p, const char* req, char** c_resp)
  {
    Client* client = reinterpret_cast<Client*>(self_p);
    try {
      std::string resp = client->calls(req);
      *c_resp = (char*)malloc(resp.length() + 1);
      memcpy(*c_resp, resp.c_str(), resp.length() + 1);
      return 0;
    } catch (ServiceCallTimeout& to) {
      // can't let exceptions escape from the C interface
      *c_resp = NULL;
      return -1;
    }
  }
}
