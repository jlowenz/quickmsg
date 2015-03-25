#pragma once

#include <quickmsg/types.hpp>
#include <zyre.h>
#include <vector>
#include <list>
#include <map>
#include <tbb/concurrent_unordered_map.h>
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
  typedef std::list<PeerPtr> PeerList;
  typedef boost::shared_ptr<PeerList> PeerListPtr;

  typedef std::add_pointer<void(const MessagePtr&,void*)>::type MessageCallback;

  class GroupNode 
  {
  public:    
    GroupNode(const std::string& description = "", bool promiscuous=false);
    virtual ~GroupNode();
    
    void join(const std::string& group);
    void wait_join(const std::string& group);
    void leave(const std::string& group);
    
    void register_handler(const std::string& group, MessageCallback cb, void* args);
    void register_whispers(MessageCallback cb, void* args);

    void shout(const std::string& group, const std::string& msg);
    void whisper(const PeerPtr peer, const std::string& msg);

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

    static std::string name();

  protected:
    void handle_whisper(const std::string& uuid, zmsg_t* msg);
    void handle_shout(const std::string& group, zmsg_t* msg);

  private:
    bool spin_once();
    
    zyre_t* node_;
    std::string node_name_;
    PeerPtr self_;
    //PeerListPtr peers_;
    //std::map<std::string,PeerListPtr> peers_by_desc_;
    std::thread* event_thread_;
    
    typedef std::unique_lock<std::mutex> basic_lock;
    typedef std::map<std::string,uint> join_map_t;
    join_map_t joins_;
    // should wrap these in a more useful class!
    std::condition_variable join_cond_;
    std::mutex join_mutex_;

    // topic -> handler
    typedef tbb::concurrent_unordered_multimap<std::string,std::pair<MessageCallback,void*> > handlers_t;
    // typedef std::map<std::string,std::pair<MessageCallback,void*> > handlers_t;
    handlers_t handlers_;
    std::pair<MessageCallback,void*> whisper_handler_;

    //static std::mutex  name_mutex_;
    static std::string name_;
    static std::atomic_bool running_;

    friend void init(const std::string&);
    friend void shutdown(const std::string&);
  };

  
}
