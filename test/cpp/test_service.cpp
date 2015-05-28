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
  virtual std::string service_impl(const qm::Message* req)
  {
    qm::AddNInts add_ints;
    return add_ints.create_resp(req->msg);
  }
};

int
main(int argc, char** argv)
{
  qm::init("test_service");
  ServiceImpl svc("add", 20); // overridden svc impl
  svc.spin();
  return 0;
}
