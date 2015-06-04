#include <glog/logging.h>
#include <quickmsg/quickmsg.hpp>
#include <quickmsg/group_node.hpp>
#include <signal.h>
#include <pthread.h>
#include <czmq.h>
#include <stdlib.h>

namespace quickmsg {

  static void (*prev_handler)(int);
  static volatile char blah = 0;
  
  void __sigint_handler(int param)
  {
    blah = 1;
    quickmsg::GroupNode::running_.store(false);
    struct sigaction action;
    struct sigaction prev_action;
    action.sa_handler = SIG_DFL;
    action.sa_flags = 0;
    sigemptyset (&action.sa_mask);
    if (sigaction (SIGINT, &action, NULL)) {
      LOG(WARNING) << "problem with sigaction" << std::endl;
    }    
    kill(0, SIGINT);
  }
  
  void init(const std::string& name)
  {
    GroupNode::name_ = name;
    zsys_init();
    //zsys_handler_set(NULL);
    
    DLOG(INFO) << "installing sig handler" << std::endl;
    struct sigaction action;
    struct sigaction prev_action;
    action.sa_handler = __sigint_handler;
    action.sa_flags = 0;
    sigemptyset (&action.sa_mask);
    if (sigaction (SIGINT, &action, NULL)) {
      LOG(WARNING) << "problem with sigaction" << std::endl;
    }

    // DLOG(INFO) << "blah before: " << blah << std::endl;
    // //raise(SIGINT);
    // DLOG(INFO) << "blah after: " << blah << std::endl;
    
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
