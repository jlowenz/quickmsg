#include <quickmsg/quickmsg_wrap.h>
#include <quickmsg/publisher_wrap.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <stdbool.h>

int
main(int argc, char** argv)
{
  int i = 0;
  char msg[256];
  qm_publisher_t p;
  const char* iface = ""; /* whatever iface zyre defaults to */
  bool handle_sig = true;
  qm_init("test_c_pub", iface, handle_sig);
  p = qm_publisher_new("chatter", qm_wait);
  for (i = 0; i < 10; ++i) {
    sprintf(msg, "Hello World %d", i);
    qm_publish(p, msg);
    msleep(500);
  }
  qm_publisher_destroy(p);
  qm_shutdown("done");
  return 0;
}
