#include <quickmsg/quickmsg_wrap.h>
#include <quickmsg/subscriber_wrap.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdbool.h>

void callback(const qm_message_t m, void* args)
{
  const char* str = qm_get_message_str(m);
  printf("C async subscriber got %s\n", str);
}

int
main(int argc, char** argv)
{
  qm_async_subscriber_t s;

  const char* iface = ""; /* whatever iface zyre defaults to */
  bool handle_sig = true;
  qm_init("test_c_sub", iface, handle_sig);
  s = qm_async_subscriber_new("chatter", callback, NULL);
  qm_async_subscriber_spin(s); /* doesn't return */
  qm_async_subscriber_destroy(s);
  qm_shutdown("done");

  return 0;
}
