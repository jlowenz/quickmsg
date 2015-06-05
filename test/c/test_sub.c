#include <quickmsg/quickmsg_wrap.h>
#include <quickmsg/subscriber_wrap.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

void callback(const qm_message_t m, void* args)
{
  const char* str = qm_get_message_str(m);
  printf("C async subscriber got %s\n", str);
}

int
main(int argc, char** argv)
{
  char msg[256];
  qm_async_subscriber_t s;
  
  qm_init("test_sub");
  s = qm_async_subscriber_new("test", callback, NULL);
  qm_async_subscriber_spin(s);
  qm_async_subscriber_destroy(s);

  return 0;
}
