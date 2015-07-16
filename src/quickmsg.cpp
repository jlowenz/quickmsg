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
	void __sigint_handler(int param)
	{
		quickmsg::GroupNode::running_.store(false);
#if _WIN32
		signal(SIGINT, SIG_DFL);
		raise(SIGINT);
#else // __WIN32
		struct sigaction action;
		struct sigaction prev_action;
		action.sa_handler = SIG_DFL;
		action.sa_flags = 0;
		sigemptyset(&action.sa_mask);
		if (sigaction(SIGINT, &action, NULL)) {
			BOOST_LOG_TRIVIAL(warning) << "problem with sigaction" << std::endl;
		}
		kill(0, SIGINT);
#endif
	}

	void __sigterm_handler(int param)
	{
		printf("sigterm before %d\n", quickmsg::GroupNode::running_.load());
		quickmsg::GroupNode::running_.store(false);
		printf("sigterm after  %d\n", quickmsg::GroupNode::running_.load());
		printf("handler running_ %lu\n", &GroupNode::running_);
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
			Beep(750, 300);
			__sigterm_handler(0);
			printf("Ctrl-C event again %d\n\n", quickmsg::ok());
			return(TRUE);

			// CTRL-CLOSE: confirm that the user wants to exit. 
		case CTRL_CLOSE_EVENT:
			Beep(600, 200);
			printf("Ctrl-Close event\n\n");
			__sigterm_handler(0);
			return(TRUE);

			// Pass other signals to the next handler. 
		case CTRL_BREAK_EVENT:
			Beep(900, 200);
			printf("Ctrl-Break event\n\n");
			__sigterm_handler(0);
			return TRUE;

		case CTRL_LOGOFF_EVENT:
			Beep(1000, 200);
			printf("Ctrl-Logoff event\n\n");
			return FALSE;

		case CTRL_SHUTDOWN_EVENT:
			Beep(750, 500);
			printf("Ctrl-Shutdown event\n\n");
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
    boost::log::core::get()->set_filter(
					boost::log::trivial::severity >= boost::log::trivial::debug);
    
    BOOST_LOG_TRIVIAL(debug) << "installing sig handler" << std::endl;
#if _WIN32
		SetConsoleCtrlHandler((PHANDLER_ROUTINE)CtrlHandler, TRUE);
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
		std::cerr << "Shutting down" << std::endl;
    GroupNode::running_.store(false);
		GroupNode::notify_interrupt();
		zsys_shutdown(); // use a sledgehammer
  }

  bool ok()
  {
		printf("running_ %lu\n", &GroupNode::running_);
		printf("quickmsg::ok %d\n\n", GroupNode::running_.load());
    return !zsys_interrupted && GroupNode::running_.load();
  }
}
