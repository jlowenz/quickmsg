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
  qm::init("test_client");
  qm::Client client("add");

  std::vector<int> add_ints_vec;
  add_ints_vec.push_back(1);
  add_ints_vec.push_back(22);
  add_ints_vec.push_back(99);
  for (int i = 0; i < 5; ++i) {
    if (!qm::ok()) break;    
    qm::AddNInts add_ints;
    std::string req_str = add_ints.create_req(add_ints_vec);
    std::cout << "client request\n" << req_str << std::endl;
    client.call_srv(req_str);
    sleep(1);
  }
  
  return 0;
}
