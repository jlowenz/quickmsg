#include <quickmsg/client_wrap.h>
#include <quickmsg/quickmsg_wrap.h>
#include <stdio.h>
#include <time.h>
#include <stdbool.h>

int
main(int argc, char** argv)
{
  /* No JSON to make it easier here */
  int i = 0;
  qm_client_t c;
  const char* iface = ""; /* whatever iface zyre defaults to */
  const char* req = "Hello";
  bool handle_sig = true;
  qm_init("test_c_client", iface, handle_sig);
  c = qm_client_new("hello");

  for (i = 0; i < 10; ++i) {
    char* resp = NULL;
    if (!qm_ok()) break;
    if (qm_call_srv(c, req, &resp)) {
      printf("ERROR: problem calling service");
    }
    printf("Received response: %s\n", resp);
    free(resp);
    msleep(1000);
  }

  qm_client_destroy(c);
  qm_shutdown(": done");
  return 0;
}
