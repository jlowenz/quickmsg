#pragma once

#include <quickmsg/types.hpp>
#include <vector>
#include <map>
#include <type_traits>
#include <stdexcept>
#include <atomic>
#include <condition_variable>
#include <thread>

namespace quickmsg {

  class Peer 
  {
  public:
    Peer(const std::string& uuid, const std::string& desc);
    virtual ~Peer();
    
    std::string uuid();
    std::string description(); // no condition on uniqueness!
  private:    
    std::string uuid_;
    std::string desc_;
  };
  typedef boost::shared_ptr<Peer> PeerPtr;
  typedef std::vector<PeerPtr> PeerList;

  // forward
  class GroupNodeImpl;

  class GroupNode
  {
  public:
    /** \brief Construct a new GroupNode for implementing group
	communication over multiple topics.
	
	Construct a new GroupNode. A GroupNode allows the owner to
	send string-based messages to groups that it has joined. A
	promiscuous GroupNode pays attention to all groups on the
	network, and joins every known group in order to report
	"off-group" communications to a specified handler. The
	promiscuous group "*" is reserved for handlers that need
	access to any known group's messages.

	\param description A string that describes this node, given as the peer name. 
	\param promiscuous Specify whether this groupnode should listen to everyone.
    */
    GroupNode(const std::string& description = "", bool promiscuous = false);
    virtual ~GroupNode();
    
    void join(const std::string& group);
    void wait_join(const std::string& group);
    void leave(const std::string& group);
    
    void register_handler(const std::string& group, MessageCallback cb, void* args);
    void register_whispers(MessageCallback cb, void* args);

    // TODO: what about de-registering the handlers???
    //void deregister_handler(const std::string& group, MessageCallback cb, void* args);
    //void deregister_handler(const std::string& group, handler_id_t handler);

    void shout(const std::string& group, const std::string& msg);
    void whisper(const PeerPtr& peer, const std::string& msg);
    void whisper(const std::string& peer_uuid, const std::string& msg);

    // return a snapshot of the peers on the network
    void peers(PeerList& ps) const;
    // return a snapshot of the peers with the given description
    void peers_by_description(PeerList& ps, const std::string& desc) const;

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

    void join();

    static std::string name();    
    
  private:
    static std::string name_;
    static std::string control_;
    static std::atomic_bool running_;
		static void notify_interrupt();

    GroupNodeImpl* self;

    friend class GroupNodeImpl;
    friend void init(const std::string&);
    friend void shutdown(const std::string&);
    friend bool ok();
    friend void __sigint_handler(int);
		friend void __sigterm_handler(int);
  };
  
}
