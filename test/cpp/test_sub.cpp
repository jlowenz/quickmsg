#include <quickmsg/quickmsg.hpp>
#include <sstream>
#include <iostream>
#include <czmq.h>
#include <zyre.h>

namespace qs = quickmsg;

class SubImpl : public qs::Subscriber
{
public:
  SubImpl(const std::string& topic, const qs::SubscriberImpl& impl, size_t queue_size=10)
    : qs::Subscriber(topic, impl, queue_size) {}
  SubImpl(const std::string& topic, size_t queue_size=10) : qs::Subscriber(topic, queue_size) {}
  virtual ~SubImpl() {}
  virtual void subscriber_impl(const std::string &msg)
  {
    std::cout<<"inherited impl"<<std::endl;
  }
};

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
  //  qs::Subscriber sub("chatter", 20);
  SubImpl sub("chatter", 20);
  while (!zsys_interrupted)
  {
    sleep(1);
  }
  return 0;
}
