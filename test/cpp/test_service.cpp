#include <quickmsg/quickmsg.hpp>
#include <quickmsg/types.hpp>
#include <sstream>
#include <iostream>
#include "add_n_ints.hpp"

namespace qm = quickmsg;

struct ServiceImpl : public qm::Service
{
  using qm::Service::Service; // Inherit ctor
  virtual ~ServiceImpl() {}
  virtual std::string service_impl(const std::string &req)
  {
    qm::AddNInts add_ints;
    return add_ints.create_resp(req);
  }
};

int
main(int argc, char** argv)
{
  qm::init("test_service");
  ServiceImpl svc("add", std::string("promisc"), 20); // overridden svc impl
  //qm::Service svc("add", std::string("promisc"), 20); // default svc impl (echo)
  svc.spin();
  return 0;
}
