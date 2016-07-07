#include <quickmsg/quickmsg.hpp>
#include <iostream>
#include <sstream>
#include <cstdio>
#include <cstdlib>
#include <thread>
#include <chrono>

namespace qm = quickmsg;

int
main(int argc, char** argv)
{
  qm::init("test_cpp_client");
  qm::Client client("hello");

  std::string req("Hello");
  for (int i = 0; i < 25; ++i) {
    if (!qm::ok()) break;    
    std::cout << "client request\n" << req << std::endl;
    std::string resp = client.calls(req);
    std::cout << i << " server response: " << resp << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(1));
  }
  
  return 0;
}
