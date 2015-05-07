#include <quickmsg/quickmsg.hpp>
#include <quickmsg/types.hpp>
#include <sstream>
#include <iostream>
#include "add_n_ints.hpp"

namespace qs = quickmsg;

struct ServiceImpl : public qs::Service
{
  using qs::Service::Service; // Inherit ctor
  virtual ~ServiceImpl() {}
  virtual std::string service_impl(const std::string &req)
  {
    qs::AddNInts add_ints;
    return add_ints.create_resp(req);
  }
};

int
main(int argc, char** argv)
{
  qs::init("test_service");
  ServiceImpl svc("add", std::string("promisc"), 20); // overridden svc impl
  //qs::Service svc("add", std::string("promisc"), 20); // default svc impl (echo)
  svc.spin();
  return 0;
}
