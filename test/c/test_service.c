#include <quickmsg/service_wrap.h>
#include <quickmsg/quickmsg_wrap.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>

char* service_handler(qm_message_t msg, void* arg)
{
  static int count = 0;
  char* resp = (char*)malloc(sizeof(char)*32);
  const char* req = qm_get_message_str(msg);
  assert(strcmp(req, "Hello") == 0);
  sprintf(resp, "World %d", count++);
  return resp;
}

int
main(int argc, char** argv)
{
  qm_service_t svc = NULL; 
  
  qm_init("test_c_service");
  svc = qm_service_new("hello", service_handler, NULL);
  qm_service_spin(svc);
  qm_service_destroy(svc);


  return 0;
}
