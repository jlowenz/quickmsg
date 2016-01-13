#include <quickmsg/quickmsg.hpp>
#include <quickmsg/types.hpp>
#include <sstream>
#include <iostream>
#include "add_n_ints.hpp"

namespace qm = quickmsg;

// Inheritance 
struct ServiceImpl : public qm::Service
{
  int msgs_recvd;
  
  ServiceImpl(std::string top, int queue_size)
    : Service(top, queue_size), msgs_recvd(0) {}
  virtual ~ServiceImpl() {}
  virtual std::string service_impl(const qm::Message* req)
  {
    std::cout << "Got: " << req->msg << std::endl;
    msgs_recvd++;
    std::stringstream ss;
    ss << "World " << msgs_recvd;
    return ss.str();
  }
};

int
main(int argc, char** argv)
{
  qm::init("test_cpp_service");
  ServiceImpl svc("hello", 20); // overridden svc impl
  svc.spin();
  return 0;
}
