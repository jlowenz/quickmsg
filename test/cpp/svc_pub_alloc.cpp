#include <quickmsg/quickmsg.hpp>
#include <quickmsg/types.hpp>
#include <sstream>
#include <iostream>
#include <chrono>
#include <thread>
#include <list>
#include <algorithm>

namespace qm = quickmsg;

// Inheritance 
struct ServiceImpl : public qm::Service
{
  int msgs_recvd;
  int work_ms_;
  std::list<qm::Publisher*> pubs_;
  
  ServiceImpl(std::string top, int work_ms, int queue_size)
    : Service(top, queue_size), msgs_recvd(0), work_ms_(work_ms) {}
  virtual ~ServiceImpl() 
  {
    std::for_each(pubs_.begin(), pubs_.end(), [](qm::Publisher* p){ delete p; });
  }
  virtual std::string service_impl(const qm::Message* req)
  {
    msgs_recvd++;
    std::cout << "Servicing request " << req->msg << " - " << msgs_recvd << std::endl;
    std::stringstream ss;
    ss << "topic: " << msgs_recvd;
    pubs_.push_back(new qm::Publisher(ss.str()));
    return ss.str();
  }
};

int
main(int argc, char** argv)
{
  qm::init("test_cpp_service");

  // read the work time from the command line
  int workms = 250;
  if (argc > 1) {
    workms = atoi(argv[1]);
  }
  ServiceImpl svc("load", workms, 20); // overridden svc impl
  svc.spin();
  return 0;
}
