#include <quickmsg/quickmsg.hpp>
#include <sstream>
#include <iostream>
#include <czmq.h>
#include <zyre.h>

namespace qm = quickmsg;

class SubImpl : public qm::Subscriber
{
public:
  SubImpl(const std::string& topic, const qm::SubscriberImpl& impl, size_t queue_size=10)
    : qm::Subscriber(topic, impl, queue_size) {}
  SubImpl(const std::string& topic, size_t queue_size=10) : qm::Subscriber(topic, queue_size) {}
  virtual ~SubImpl() {}
  virtual void subscriber_impl(const std::string &msg)
  {
    std::cout<<"inherited impl"<<std::endl;
  }
};

void
handler(const qm::MessagePtr& msg, void* args)
{
  std::cout << "Message received: " << msg->msg << std::endl;
}

int
main(int argc, char** argv)
{
  qm::init("test_sub");
  //  qm::AsyncSubscriber async_sub("chatter", &handler, NULL);  
  //  async_sub.spin();
  //  qm::Subscriber sub("chatter", 20);
  SubImpl sub("chatter", 20);
  while (qm::ok())
  {
    sleep(1);
  }
  return 0;
}
