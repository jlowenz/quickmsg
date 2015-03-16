#include <quickmsg/subscriber.hpp>

namespace quickmsg {

  Subscriber::Subscriber(const std::string& topic)
  {
  }
  
  Subscriber::~Subscriber()
  {
  }
    
  /** \brief Return messages that have arrived since the last call.
      
      \return a list of messages
  */
  MsgListPtr
  Subscriber::messages()
  {
    return boost::make_shared<MsgList>();
  }

}
