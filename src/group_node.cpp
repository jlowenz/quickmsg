#include <quickmsg/group_node.hpp>

namespace quickmsg {

  Peer::Peer(const std::string uuid)
  {
  }
  
  Peer::~Peer()
  {
  }
    
  std::string 
  Peer::uuid()
  {
    return std::string("");
  }
  
  std::string 
  Peer::description()
  {
    return std::string("");
  }

  GroupNode::GroupNode()
  {
  }
  
  GroupNode::~GroupNode()
  {
  }
    
  void   
  GroupNode::join(const std::string& group)
  {
  }
  
  void
  GroupNode::leave(const std::string& group)
  {
  }
    
  void 
  GroupNode::register_handler(const std::string& group, MessageCallback cb)
  {
  }

  void 
  GroupNode::shout(const std::string& group, const std::string& msg)
  {
  }

  void 
  GroupNode::whisper(const Peer* peer, const std::string& msg)
  {
  }

  PeerListPtr 
  GroupNode::peers() const
  {
    PeerListPtr p;
    return p;
  }

  PeerListPtr
  GroupNode::peers_by_description(const std::string& desc) const
  {
    PeerListPtr p;
    return p;
  }
  
  /** \brief Terminate any processing for this node.
      Terminate any processing for this node, non-recoverable (for now).
  */
  void 
  GroupNode::stop()
  {
  }

  /** \brief Start the listener. This method does not return.
      
      Spinning a group node is required for receiving messages
      whether you plan to access the messages synchronously OR
      asynchronously: something still must listen to the underlying
      network.
  */
  void 
  GroupNode::spin()
  {
  }

  /** \brief Start the listener thread. This method will return immediately.
      
      Spinning a group node is required for receiving messages
      whether you plan to access the messages synchronously OR
      asynchronously: something still must listen to the underlying
      network.
  */
  void 
  GroupNode::async_spin()
  {
  }
}
