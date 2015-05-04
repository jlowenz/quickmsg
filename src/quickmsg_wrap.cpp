#include <quickmsg/quickmsg.hpp>
#include <quickmsg/group_node.hpp>

using namespace quickmsg;

extern "C" {

void
qmg_init(const std::string& name) 
{
  init(name);
}

void
qmg_shutdown(const std::string& reason)
{
  shutdown(reason);
}

}
