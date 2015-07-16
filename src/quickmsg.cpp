#include <boost/log/trivial.hpp>
#include <boost/log/core.hpp>
#include <boost/log/expressions.hpp>
#include <quickmsg/quickmsg.hpp>
#include <quickmsg/group_node.hpp>
#include <signal.h>
#include <pthread.h>
#include <czmq.h>
#include <stdlib.h>

#if _WIN32
#include <WinSock2.h>
#endif

namespace quickmsg {

  static void (*prev_handler)(int);
  static volatile char blah = 0;
  
  void __sigint_handler(int param)
  {
    blah = 1;
    quickmsg::GroupNode::running_.store(false);
#if _WIN32
    signal(SIGINT, SIG_DFL);
    raise(SIGINT);
#else // __WIN32
    struct sigaction action;
    struct sigaction prev_action;
    action.sa_handler = SIG_DFL;
    action.sa_flags = 0;
    sigemptyset (&action.sa_mask);
    if (sigaction (SIGINT, &action, NULL)) {
      BOOST_LOG_TRIVIAL(warning) << "problem with sigaction" << std::endl;
    }
    kill(0, SIGINT);
#endif
  }
  
  void init(const std::string& name)
  {
    GroupNode::running_.store(true);
    GroupNode::name_ = name;
    GroupNode::control_ = name + "/CTL";
#if _WIN32
    printf("Windows: initializing WinSock\n");
    WSADATA ws_data;
    WORD wVersionRequested;
    wVersionRequested = MAKEWORD(2, 2);
    int err = WSAStartup(wVersionRequested,&ws_data);
    if (err != 0) {
      printf("WSAStartup failed with %d\n", err);
    }
#endif
    zsys_init();
    //zsys_handler_set(NULL);
    boost::log::core::get()->set_filter(
					boost::log::trivial::severity >= boost::log::trivial::debug);
    
    BOOST_LOG_TRIVIAL(debug) << "installing sig handler" << std::endl;
#if _WIN32
    signal(SIGINT, __sigint_handler);
#else
    struct sigaction action;
    struct sigaction prev_action;
    action.sa_handler = __sigint_handler;
    action.sa_flags = 0;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGINT, &action, NULL)) {
      BOOST_LOG_TRIVIAL(warning) << "problem with sigaction" << std::endl;
    }
#endif

    // BOOST_LOG_TRIVIAL(debug) << "blah before: " << blah << std::endl;
    // //raise(SIGINT);
    // BOOST_LOG_TRIVIAL(debug) << "blah after: " << blah << std::endl;
    
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
