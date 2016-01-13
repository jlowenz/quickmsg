#include <quickmsg/quickmsg.hpp>
#include <iostream>
#include <sstream>
#include <cstdio>
#include <cstdlib>
#include <chrono>
#include <thread>

namespace qm = quickmsg;

int
main(int argc, char** argv)
{
  qm::init("test_cpp_pub");
  
  qm::Publisher pub("chatter", true);
  
  for (int i = 0; i < 20; ++i) {
    if (!qm::ok()) break;
    std::stringstream ss;  
    ss << "Hello world: " << i << std::endl;
    pub.publish(ss.str());    
    std::cout << ss.str();
    std::this_thread::sleep_for(std::chrono::seconds(1));
  }
  return 0;
}
