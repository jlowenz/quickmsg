#include <quickmsg/quickmsg.hpp>
#include <sstream>
#include <iostream>

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
  qs::AsyncSubscriber sub("chatter", &handler, NULL);  
  sub.spin();
  return 0;
}
