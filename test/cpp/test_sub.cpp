#include <quickmsg/quickmsg.hpp>
#include <sstream>
#include <iostream>
#include <czmq.h>
#include <zyre.h>

namespace qs = quickmsg;

void
handler(const qs::MessagePtr& msg, void* args)
{
  std::cout << "Message received: " << msg->msg << std::endl;
}

int
main(int argc, char** argv)
{
  qs::init("test_sub");
  //  qs::AsyncSubscriber async_sub("chatter", &handler, NULL);  
  //  async_sub.spin();
  qs::Subscriber sub("chatter", 20);
  while (!zsys_interrupted)
  {
    sleep(1);
  }
  return 0;
}
