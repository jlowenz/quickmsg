#include <quickmsg/quickmsg.hpp>
#include <iostream>
#include <sstream>
#include <cstdio>
#include <cstdlib>
#include <boost/date_time/posix_time/posix_time.hpp>

namespace qm = quickmsg;
namespace pt = boost::posix_time;

int
main(int argc, char** argv)
{
  qm::init("test_cpp_client");

  qm::Client client("load");
  std::string req("query");

  pt::ptime start = pt::microsec_clock::local_time();
  for (int i = 0; i < 100; i++) {
    if (!qm::ok()) break;
    std::cout << "client request\n" << req << std::endl;
    std::string resp = client.calls(req);
    std::cout << i << " server response: " << resp << std::endl;
  }
  auto diff = pt::microsec_clock::local_time() - start;
  
  std::cout << "Total time: " << pt::to_simple_string(diff) << std::endl;

  return 0;
}
