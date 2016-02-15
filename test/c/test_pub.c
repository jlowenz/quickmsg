#include <quickmsg/quickmsg_wrap.h>
#include <quickmsg/publisher_wrap.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

int
main(int argc, char** argv)
{
  int i = 0;
  char msg[256];
  qm_publisher_t p;
  const char* iface = ""; /* whatever iface zyre defaults to */

#ifndef _WIN32
  struct timespec ts;
  ts.tv_sec = 1;
  ts.tv_nsec = 0;
#endif

  qm_init("test_c_pub", iface);
  p = qm_publisher_new("chatter", qm_wait);
  for (i = 0; i < 10; ++i) {
    sprintf(msg, "Hello World %d", i);
    qm_publish(p, msg);
#ifdef _WIN32
    Sleep(1000);
#else
    nanosleep(&ts, NULL);
#endif
  }
  qm_publisher_destroy(p);
  qm_shutdown("done");
  return 0;
}
