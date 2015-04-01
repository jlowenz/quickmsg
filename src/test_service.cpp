#include <quickmsg/quickmsg.hpp>
#include <quickmsg/add_n_ints.hpp>
#include <quickmsg/types.hpp>
#include <sstream>
#include <iostream>

namespace qs = quickmsg;

std::string add_ints_impl(const qs::MessagePtr& req)
{
  qs::AddNInts add_ints;
  return add_ints.create_resp(req->msg);
}

int
main(int argc, char** argv)
{
  qs::init("test_service");
  qs::Service svc("add", add_ints_impl, std::string("promisc"), 20);
  svc.spin();
  return 0;
}
