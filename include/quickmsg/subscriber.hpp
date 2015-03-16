#pragma once

#include <quickmsg/types.hpp>
#include <quickmsg/group_node.hpp>
#include <boost/shared_ptr.hpp>
#include <list>

namespace quickmsg {
  
  class Subscriber
  {
  public:
    Subscriber(const std::string& topic);
    virtual ~Subscriber();
    
    /** \brief Return messages that have arrived since the last call.
	
	\return a list of messages
     */
    MsgListPtr messages();
    
  private:
    GroupNode* node_;
  };

}
