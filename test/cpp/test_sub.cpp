#include <quickmsg/quickmsg.hpp>
#include <sstream>
#include <iostream>
#include <czmq.h>
#include <zyre.h>

namespace qm = quickmsg;

class SubImpl : public qm::AsyncSubscriber
{
public:

  SubImpl(const std::string& topic) : qm::AsyncSubscriber(topic) {}
  virtual ~SubImpl() {}
  virtual void handle_message(const qm::Message* msg)
  {
    std::cout<<"inherited impl"<<std::endl;
    std::cout << "msg: " << msg->msg << std::endl;
  }
};

void
handler(const qm::Message* msg, void* args)
{
  std::cout << "Message received: " << msg->msg << std::endl;
}

int
main(int argc, char** argv)
{
  qm::init("test_sub");
  qm::AsyncSubscriber async_sub("chatter", handler, NULL);  
  async_sub.spin();
  //  qm::Subscriber sub("chatter", 20);
  // SubImpl sub("chatter");
  // while (qm::ok())
  // {
  //   sleep(1);
  // }
  return 0;
}
