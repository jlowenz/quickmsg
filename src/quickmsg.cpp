#include <glog/logging.h>
#include <quickmsg/quickmsg.hpp>
#include <quickmsg/group_node.hpp>
#include <csignal>


namespace quickmsg {

  static void (*prev_handler)(int);

  void __sigint_handler(int param)
  {
    std::cerr << "WTF" << std::endl;
    DLOG(INFO) << "quickmsg sigint handler" << std::endl;
    quickmsg::GroupNode::running_.store(false);
    prev_handler(param);
  }
  
  void init(const std::string& name)
  {
    GroupNode::name_ = name;
    prev_handler = std::signal(SIGINT, __sigint_handler);
  }

  void shutdown(const std::string& reason)
  {
    GroupNode::running_.store(false);
  }

  bool ok()
  {
    return !zsys_interrupted && GroupNode::running_.load();
  }
}
