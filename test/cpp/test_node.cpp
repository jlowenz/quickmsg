#include <boost/log/trivial.hpp>
#include <quickmsg/service.hpp>
#include <quickmsg/group_node.hpp>
#include <quickmsg/types.hpp>
#include <quickmsg/quickmsg.hpp>
#include <tbb/concurrent_queue.h>
#include <type_traits>
#include <sstream>
#include <string.h>
#include <iostream>

using namespace quickmsg;

void service_handler(const Message* msg, void* args)
{
  static int count = 0;
  GroupNode* node = (GroupNode*)args;
  std::stringstream resp;
  resp << "World " << count++;
  std::string src = msg->get_src();
  std::cout << "whispering..."<< std::endl;
  node->whispers(src, resp.str());
  std::cout<<"done."<<std::endl;
}

int
main(int argc, char** argv)
{
  init("test_cpp_node");
  GroupNode node("SRV/svc_node", true);
  node.join("hello");
  node.register_handler("hello", &service_handler, &node);
  node.async_spin();
  node.join();  
  return 0;
}
