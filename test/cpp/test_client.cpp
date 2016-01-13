#include <quickmsg/quickmsg.hpp>
#include <iostream>
#include <sstream>
#include <cstdio>
#include <cstdlib>
#include <zyre.h>
#include "add_n_ints.hpp"

namespace qm = quickmsg;

int
main(int argc, char** argv)
{
  qm::init("test_cpp_client");
  qm::Client client("hello");

  std::string req("Hello");
  for (int i = 0; i < 10; ++i) {
    if (!qm::ok()) break;    
    std::cout << "client request\n" << req << std::endl;
    std::string resp = client.calls(req);
    std::cout << "server response: " << resp << std::endl;
    sleep(1);
  }
  
  return 0;
}
