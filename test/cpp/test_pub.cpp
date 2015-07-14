#include <quickmsg/quickmsg.hpp>
#include <iostream>
#include <sstream>
#include <cstdio>
#include <cstdlib>

namespace qm = quickmsg;

int
main(int argc, char** argv)
{
  qm::init("test_pub");
  
  qm::Publisher pub("chatter", true);
  
  for (int i = 0; i < 20; ++i) {
    if (!qm::ok()) break;
    std::stringstream ss;  
    ss << "Hello world: " << i << std::endl;
    pub.publish(ss.str());    
    std::cout << ss.str();
    sleep(1);
  }
  return 0;
}
