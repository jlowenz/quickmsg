#pragma once

#include <quickmsg/types.hpp>
#include <zyre.h>
#include <vector>
#include <list>
#include <map>
#include <type_traits>

namespace quickmsg {

  class Peer 
  {
  public:
    Peer(const std::string uuid);
    virtual ~Peer();
    
    std::string uuid();
    std::string description(); // no condition on uniqueness!
  private:    
  };
  typedef boost::shared_ptr<Peer> PeerPtr;
  typedef std::list<PeerPtr> PeerList;
  typedef boost::shared_ptr<PeerList> PeerListPtr;

  void _handle_message(const std::string& msg);
  
  typedef std::add_pointer<decltype(_handle_message)> MessageCallback;

  class GroupNode 
  {
  public:
    GroupNode();
    virtual ~GroupNode();
    
    void join(const std::string& group);
    void leave(const std::string& group);
    
    void register_handler(const std::string& group, MessageCallback cb);

    void shout(const std::string& group, const std::string& msg);
    void whisper(const Peer* peer, const std::string& msg);

    PeerListPtr peers() const;
    PeerListPtr peers_by_description(const std::string& desc) const;

    /** \brief Terminate any processing for this node.
	Terminate any processing for this node, non-recoverable (for now).
    */
    void stop();

    /** \brief Start the listener. This method does not return.
	
	Spinning a group node is required for receiving messages
	whether you plan to access the messages synchronously OR
	asynchronously: something still must listen to the underlying
	network.
    */
    void spin();

    /** \brief Start the listener thread. This method will return immediately.
	
	Spinning a group node is required for receiving messages
	whether you plan to access the messages synchronously OR
	asynchronously: something still must listen to the underlying
	network.
    */
    void async_spin();
  private:
    zyre_t* node_;
    PeerListPtr peers_;
    std::map<std::string,PeerListPtr> peers_by_desc_;
  };
  
}
