#include <quickmsg/quickmsg.hpp>
#include <quickmsg/group_node.hpp>

namespace quickmsg {
  
  void init(const std::string& name)
  {
    GroupNode::name_ = name;
  }

  void shutdown(const std::string& reason)
  {
    GroupNode::running_.store(false);
  }

  bool ok()
  {
    return !zsys_interrupted;
  }
}
