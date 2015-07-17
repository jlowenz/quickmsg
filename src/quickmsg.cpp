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

//   void __sigint_handler(int param)
//   {
//     quickmsg::GroupNode::running_.store(false);
// #if _WIN32
//     signal(SIGINT, SIG_DFL);
//     raise(SIGINT);
// #else // __WIN32
//     struct sigaction action;
//     struct sigaction prev_action;
//     action.sa_handler = SIG_DFL;
//     action.sa_flags = 0;
//     sigemptyset(&action.sa_mask);
//     if (sigaction(SIGINT, &action, NULL)) {
//       BOOST_LOG_TRIVIAL(warning) << "problem with sigaction" << std::endl;
//     }
//     kill(0, SIGINT);
// #endif
//   }

  void __shutdown_handler(int param)
  {
    quickmsg::GroupNode::running_.store(false);
    quickmsg::GroupNode::notify_interrupt();
  }

#if _WIN32
  BOOL CtrlHandler(DWORD fdwCtrlType)
  {
    switch (fdwCtrlType)
      {
	// Handle the CTRL-C signal. 
      case CTRL_C_EVENT:
	printf("Ctrl-C event %d\n\n", quickmsg::ok());
	__shutdown_handler(0);
	return(TRUE);

	// CTRL-CLOSE: confirm that the user wants to exit. 
      case CTRL_CLOSE_EVENT:
	Beep(600, 200);
	printf("Ctrl-Close event\n\n");
	__shutdown_handler(0);
	return(TRUE);

	// Pass other signals to the next handler. 
      case CTRL_BREAK_EVENT:
	Beep(900, 200);
	printf("Ctrl-Break event\n\n");
	__shutdown_handler(0);
	return TRUE;

      case CTRL_LOGOFF_EVENT:
	Beep(1000, 200);
	printf("Ctrl-Logoff event\n\n");
	__shutdown_handler(0);
	return FALSE;

      case CTRL_SHUTDOWN_EVENT:
	Beep(750, 500);
	printf("Ctrl-Shutdown event\n\n");
	__shutdown_handler(0);
	return FALSE;

      default:
	return FALSE;
      }
  }
#endif

  static void (*prev_handler)(int);

  void init(const std::string& name)
  {
    GroupNode::running_.store(true);
    GroupNode::name_ = name;
    GroupNode::control_ = name + "/CTL";
    zsys_init();
    //zsys_handler_set(NULL);
    boost::log::core::get()
      ->set_filter(boost::log::trivial::severity >= boost::log::trivial::debug);
    
    BOOST_LOG_TRIVIAL(debug) << "installing sig handler" << std::endl;
#if _WIN32
    SetConsoleCtrlHandler((PHANDLER_ROUTINE)CtrlHandler, TRUE);
#else
    struct sigaction action;
    struct sigaction prev_action;
    action.sa_handler = __shutdown_handler;
    action.sa_flags = 0;
    sigemptyset(&action.sa_mask);
    if (sigaction(SIGINT, &action, NULL)) {
      BOOST_LOG_TRIVIAL(warning) << "problem with sigaction" << std::endl;
    }
#endif

  }

  void shutdown(const std::string& reason)
  {
    //GroupNode::running_.store(false);
    //GroupNode::notify_interrupt();
    zsys_shutdown(); // use a sledgehammer
  }

  bool ok()
  {
    return !zsys_interrupted && GroupNode::running_.load();
  }
}
