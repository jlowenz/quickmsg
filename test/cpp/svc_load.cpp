#include <quickmsg/quickmsg.hpp>
#include <quickmsg/types.hpp>
#include <sstream>
#include <iostream>
#include <chrono>
#include <thread>

namespace qm = quickmsg;

// Inheritance 
struct ServiceImpl : public qm::Service
{
  int msgs_recvd;
  int work_ms_;
  
  ServiceImpl(std::string top, int work_ms, int queue_size)
    : Service(top, queue_size), msgs_recvd(0), work_ms_(work_ms) {}
  virtual ~ServiceImpl() {}
  virtual std::string service_impl(const qm::Message* req)
  {
    std::cout << "Servicing request " << req->msg << std::endl;
    std::this_thread::sleep_for(std::chrono::milliseconds(work_ms_));
    std::cout << "done." << std::endl;
    msgs_recvd++;
    std::stringstream ss;
    ss << "Msgs received: " << msgs_recvd;
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
