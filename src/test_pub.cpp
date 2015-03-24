#include <quickmsg/quickmsg.hpp>
#include <iostream>
#include <sstream>
#include <cstdio>
#include <cstdlib>

namespace qs = quickmsg;

int
main(int argc, char** argv)
{
  qs::init("test_pub");
  
  qs::Publisher pub("chatter");
  
  for (int i = 0; i < 20; ++i) {    
    std::stringstream ss;  
    ss << "Hello world: " << i << std::endl;
    pub.publish(ss.str());    
    std::cout << ss.str();
    sleep(1);
  }
  
  return 0;
}
