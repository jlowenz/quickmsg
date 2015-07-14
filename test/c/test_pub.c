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
  struct timespec ts;
  ts.tv_sec = 1;
  ts.tv_nsec = 0;

  qm_init("test_pub");
  p = qm_publisher_new("test", qm_wait);
  for (i = 0; i < 10; ++i) {
    sprintf(msg, "Hello World %d", i);
    qm_publish(p, msg);
    nanosleep(&ts, NULL);
  }
  qm_publisher_destroy(p);
  return 0;
}
