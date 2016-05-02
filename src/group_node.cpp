#include <iterator> 
#include <thread>
#include <chrono>
#include <functional>
#include <algorithm>
#include <iostream>
#include <sstream>
#include <signal.h>
#include <zyre.h>

#include <tbb/concurrent_unordered_map.h>

#include <boost/log/trivial.hpp>
#include <boost/foreach.hpp>
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/split.hpp>

#include <quickmsg/quickmsg.hpp>
#include <quickmsg/group_node.hpp>

#ifndef _WIN32
#  include <pthread.h>
#else
#  undef interface
#endif

#define DEBUG

namespace quickmsg {

  void parse_string_csv(const std::string& csv, std::vector<std::string>& v)
  {
    std::vector<std::string> nums;
    boost::split(nums, csv, boost::is_any_of(","), boost::token_compress_on);
    BOOST_FOREACH (std::string& s, nums) {
      v.push_back(s);
    }
  }


  // static_assert(std::is_trivially_copyable<std::string>::value, 
  // 		"std::atomic<std::string> requires std::string to be trivially copyable");

  // we will not protect name_ since it will be set once, and otherwise remain read-only

  Peer::Peer(const std::string& uuid, const std::string& desc)
    : uuid_(uuid), desc_(desc)
  {
  }
  
  Peer::~Peer()
  {
  }
    
  std::string 
  Peer::uuid()
  {
    return uuid_;
  }
  
  std::string 
  Peer::description()
  {
    return desc_;
  }

  void GroupNode::notify_interrupt()
  {
    GroupNode node("_interrupter_");
    node.join(GroupNode::control_);
		std::this_thread::sleep_for(std::chrono::milliseconds(500));
		node.leave(GroupNode::control_);
		node.stop();
  }

  //--------------------------------------------------------------------------------
  // define the private data implementation
  class GroupNodeImpl 
  {
  public:    

    GroupNodeImpl(const std::string& description = "", bool promiscuous=false);
    virtual ~GroupNodeImpl();
    
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

    // return whether node has been interrupted
    bool interrupted();
    void join();


    static std::string name();

    void handle_whisper(const std::string& uuid, zmsg_t* msg);
    void handle_shout(const std::string& group, const std::string& uuid, zmsg_t* msg);

    bool spin_once();
    void update_groups();
    void _spin(); // for async, signal-disabled spinning
    
  private:

    zyre_t* node_;
    std::string node_name_;
    std::string control_group_;
    std::string node_iface_;
    std::string node_desc_;
    PeerPtr self_;
    std::thread* event_thread_;
    std::thread* prom_thread_;

    bool promiscuous_;
    bool stopped_;
    
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
    handlers_t whisper_handlers_;    

    //static std::mutex  name_mutex_;
    // static std::string name_;
    // static std::atomic_bool running_;

    // friend void init(const std::string&);
    // friend void shutdown(const std::string&);
    // friend bool ok();
    // friend void __sigint_handler(int);
  };

  std::string GroupNode::name_("");
  std::string GroupNode::iface_("");
  std::string GroupNode::control_("");
  std::atomic_bool GroupNode::running_;

  std::string GroupNode::name()
  {
    return GroupNode::name_;
  }

  std::string GroupNode::iface()
  {
    return GroupNode::iface_;
  }

  GroupNodeImpl::GroupNodeImpl(const std::string& desc, bool promiscuous)
    : event_thread_(NULL), prom_thread_(NULL), promiscuous_(promiscuous), stopped_(false)
  {							
    node_name_ = GroupNode::name() + "/" + desc;
    std::vector<std::string> node_ifaces_;
    parse_string_csv(GroupNode::iface(), node_ifaces_);
    size_t ifaces_to_try = (GroupNode::iface().empty()) ? 0 : node_ifaces_.size();
    node_desc_ = desc;
    control_group_ = GroupNode::control_;

    // start the node
    size_t interface = 0;
    do {
      if (ifaces_to_try > 0 && interface >= node_ifaces_.size()) {
	std::ostringstream ss;
	ss << "Could not start zyre node. Possibly due to bad broadcast interface (" << GroupNode::iface() << ")";
	throw std::runtime_error(ss.str());
      }
      if (interface > 0) {
	BOOST_LOG_TRIVIAL(warning) << "Zyre node failed to start with interface: " << node_iface_ << std::endl;
	BOOST_LOG_TRIVIAL(warning) << "  Trying again with: " << node_ifaces_[interface] << std::endl;
	zyre_destroy(&node_);
      }
      // create the zyre node
      node_ = zyre_new(node_name_.c_str());
      // verbose output
      // zyre_set_verbose(node_);
      // set the headers
      zyre_set_header(node_, "desc", "%s", node_desc_.c_str());
      // access our uuid
      std::string uuid = zyre_uuid(node_);
      BOOST_LOG_TRIVIAL(info) << "Node UUID: " << uuid << std::endl;
      // create our self peer
      self_.reset(new Peer(uuid, node_desc_));
      // access our name
      node_name_ = zyre_name(node_);

      // set the interface to use, otherwise zyre just guesses (default "")
      if (node_ifaces_.size() > 0) {
	zyre_set_interface(node_, node_ifaces_[interface].c_str());
	node_iface_ = node_ifaces_[interface];
      } else {
	// this seems to be the only reasonable default, 
	// despite the documentation for zsys_set_interface()
	zyre_set_interface(node_, "");
	node_iface_ = "";
      }

      interface++;
    } while (zyre_start(node_) != 0);

    // join the control group - too heavy
    if (zyre_join(node_, control_group_.c_str())) {
      throw std::runtime_error("Error joining CONTROL group");
    }


    /* we want to minimize the groups joined and messages sent -
       especially by other non-promiscuous components; in order to do
       so, we need to periodically join all the known groups.
     */
    if (promiscuous_) {
      prom_thread_ = new std::thread(&GroupNodeImpl::update_groups, this);
      BOOST_LOG_TRIVIAL(debug) << "Started promiscuous group thread..." << std::endl;
    }
  }
  GroupNode::GroupNode(const std::string& desc, bool prom)
    : self(new GroupNodeImpl(desc, prom))
  {    
  }
  
  GroupNodeImpl::~GroupNodeImpl()
  {    
    if (!stopped_) {
      stopped_ = true;
      zyre_stop(node_);
    }
    if (prom_thread_) {
      if (prom_thread_->joinable()) {
	prom_thread_->join();
      }
      delete prom_thread_;
    }
    if (event_thread_) {
      if (event_thread_->joinable()) {
	event_thread_->join();
      }
      delete event_thread_;
    }
    BOOST_LOG_TRIVIAL(debug) << "destroying zyre node..." << std::endl;
    zyre_destroy(&node_);
  }
  GroupNode::~GroupNode()
  {
    delete self;
  }
    
  void    
  GroupNodeImpl::join(const std::string& group)
  {  
    {
      basic_lock lk(join_mutex_);
      joins_[group] = 0;    
    }
    if (zyre_join(node_, group.c_str())) {
      throw std::runtime_error("Error joining group: " + group);
    }
    // we should WAIT to get an enter event?
  }
  void
  GroupNode::join(const std::string& group)
  {
    self->join(group);    
  }

  void
  GroupNodeImpl::wait_join(const std::string& group)
  {
    // how do we wait for joins? we should receive at least one join message
    // but we MAY have received it in the PAST! 
    std::chrono::duration<int,std::milli> period(500);
    basic_lock lk(join_mutex_); 
    if (joins_[group] > 0) return;
    else {
      while (joins_[group] == 0 && ok()) {
	// check periodically to make sure we're still running
	join_cond_.wait_for(lk, period);
      }
    }
  }
  void
  GroupNode::wait_join(const std::string& group)
  {
    self->wait_join(group);
  }
  
  void
  GroupNodeImpl::leave(const std::string& group)
  {
    if (zyre_leave(node_, group.c_str())) {
      throw std::runtime_error("Error leaving group: " + group);
    }
  }
  void
  GroupNode::leave(const std::string& group)
  {
    self->leave(group);
  }
    
  void 
  GroupNodeImpl::register_handler(const std::string& group, MessageCallback cb, void* args)
  {
    // add to the handlers
    handlers_.insert(std::make_pair(group,std::make_pair(cb, args)));
  }
  void
  GroupNode::register_handler(const std::string& group, MessageCallback cb, void* args)
  {
    self->register_handler(group, cb, args);
  }

  void 
  GroupNodeImpl::register_whispers(MessageCallback cb, void* args)
  {
    // add to the handlers   
    whisper_handlers_.insert(std::make_pair("w",std::make_pair(cb,args)));
  }
  void
  GroupNode::register_whispers(MessageCallback cb, void* args)
  {
    self->register_whispers(cb, args);
  }


  void 
  GroupNodeImpl::shout(const std::string& group, const std::string& msg)
  {
    //zmsg_t* zmsg = zmsg_new();
    //zmsg_pushstr(zmsg, msg.c_str());
    const char* g = group.c_str();
    const char* m = msg.c_str();
    BOOST_LOG_TRIVIAL(debug) << "shouting to group [" << g << "] message: [" << m << "]" << std::endl;
    if (zyre_shouts(node_, group.c_str(), "%s", msg.c_str())) {
      throw std::runtime_error("Error sending message to topic: " + group);
    }
  }
  void
  GroupNode::shout(const std::string& group, const std::string& msg)
  {
    self->shout(group, msg);
  }

  void 
  GroupNodeImpl::whisper(const std::string& peer_uuid, const std::string& msg)
  {
    BOOST_LOG_TRIVIAL(debug) << "GroupNodeImpl::whisper" << std::endl;
    if (zyre_whispers(node_, peer_uuid.c_str(), "%s", msg.c_str())) {
      throw std::runtime_error("Error sending message to peer: " + peer_uuid);
    }
  }
  void
  GroupNode::whispers(const std::string& peer_uuid, const std::string& msg)
  {
    self->whisper(peer_uuid, msg);
  }

  void 
  GroupNodeImpl::whisper(const PeerPtr& peer, const std::string& msg)
  {
    if (zyre_whispers(node_, peer->uuid().c_str(), "%s", msg.c_str())) {
      throw std::runtime_error("Error sending message to peer: " + peer->uuid());
    }
  }
  void 
  GroupNode::whisper(const PeerPtr& peer, const std::string& msg)
  {
    self->whisper(peer, msg);
  }


  void
  GroupNodeImpl::peers(PeerList& ps) const
  {
    ps.clear();
    zlist_t* zpeers = zyre_peers(node_);
    char* uuid;
    while ((uuid = static_cast<char*>(zlist_next(zpeers))) != NULL) {
      char* desc = zyre_peer_header_value(node_, uuid, "desc");
      PeerPtr p(new Peer(uuid, desc));
      ps.push_back(p);
      free(desc);
    }
    zlist_destroy(&zpeers);
  }
  void 
  GroupNode::peers(PeerList& ps) const
  {
    self->peers(ps);
  }

  void
  GroupNodeImpl::peers_by_description(PeerList& ps, const std::string& desc) const
  {
    PeerList tmp_ps;
    this->peers(tmp_ps);
    PeerList::iterator new_end = std::remove_if(tmp_ps.begin(), tmp_ps.end(), [&](PeerPtr& p) {
	return p->uuid() != std::string(desc); });
    std::copy(tmp_ps.begin(), new_end, std::back_inserter(ps));
  }
  void 
  GroupNode::peers_by_description(PeerList& ps, const std::string& desc) const
  {
    self->peers_by_description(ps, desc);
  }

  
  /** \brief Terminate any processing for this node.
      Terminate any processing for this node, non-recoverable (for now).
  */
  void 
  GroupNodeImpl::stop()
  {
    if (!stopped_) {
      stopped_ = true;
      zyre_stop(node_);
    }
  }
  void
  GroupNode::stop()
  {
    self->stop();
  }

  void
  GroupNodeImpl::handle_whisper(const std::string& uuid, zmsg_t* zmsg)
  {    
    MessagePtr msg(new Message);
    msg->header.stamp = time_now();
    msg->header.context = uuid;
    msg->header.src_uuid = uuid;
    char* a_str = zmsg_popstr(zmsg);
    msg->msg = a_str;
    free(a_str);
    auto range = whisper_handlers_.equal_range("w");
    std::for_each(range.first, range.second,
		  [&](handlers_t::value_type& c){c.second.first(msg.get(), c.second.second);});
  }

  void 
  GroupNodeImpl::handle_shout(const std::string& group, const std::string& uuid, zmsg_t* zmsg)
  {
    MessagePtr msg(new Message);
    msg->header.stamp = time_now();
    msg->header.context = group;
    msg->header.src_uuid = uuid;
    char* a_str = zmsg_popstr(zmsg);
    assert(a_str);
    msg->msg = std::string(a_str);
    BOOST_LOG_TRIVIAL(debug) << "handle_shout got msg [" << msg->msg << "]" << std::endl;
    free(a_str);
    //zmsg_destroy(&zmsg);
    auto range = handlers_.equal_range(group);
    std::for_each(range.first,range.second,
		  [&](handlers_t::value_type& x){x.second.first(msg.get(), x.second.second);});

    // handle promiscuity
    if (promiscuous_ && range.first == range.second) {
      range = handlers_.equal_range("*");
      std::for_each(range.first, range.second,
		    [&](handlers_t::value_type& x){x.second.first(msg.get(), x.second.second);});
    }
  }

  class ScopedEvent
  {
  public:
    ScopedEvent(zyre_event_t* e)
      : e_(e)
    {
    }

    ~ScopedEvent() 
    {
      BOOST_LOG_TRIVIAL(debug) << "DELETING SCOPED EVENT" << std::endl;
      zyre_event_destroy(&e_);
    }

    bool valid() const {
      return e_ != NULL;
    }

    zyre_event_type_t type() const {
      return zyre_event_type(e_);
    }

    std::string peer_uuid() const {
      const char* s = zyre_event_sender(e_);
      assert(s != NULL);
      return std::string(s);
    }

    std::string peer_name() const {
      const char* s = zyre_event_name(e_);
      assert(s != NULL);
      std::string str(s);
      //printf("peer_name %s", str.c_str());
      return str;
    }
    
    std::string group() const {
      const char* s = zyre_event_group(e_);
      assert(s != NULL);
      return std::string(s);
    }
    
    zmsg_t* message() const {
      return zyre_event_msg(e_);
    }

  private:
    zyre_event_t* e_;
  };

  bool 
  GroupNodeImpl::spin_once()
  {
    // read a new event from the zyre node, interrupt
    if (zsys_interrupted || !ok() || stopped_) {
      BOOST_LOG_TRIVIAL(debug) << "GroupNode::spin_once(): zsysi " << (int)zsys_interrupted << 
	" ok " << (int)!ok() << " stopped " <<  (int)stopped_ << std::endl;
      return false;
    }

    BOOST_LOG_TRIVIAL(debug) << "waiting for event" << std::endl;
    ScopedEvent e(zyre_event_new(node_)); // apparently, blocks until event occurs.
    // will be destroyed at the end of the function
    if (e.valid() && ok()) {
      BOOST_LOG_TRIVIAL(debug) << "got event" << std::endl;
      zyre_event_type_t t = e.type();
      switch (t) {
      case ZYRE_EVENT_WHISPER: {
        std::string name = e.peer_name();
        BOOST_LOG_TRIVIAL(debug) << name << " whispers -> " << node_name_ << std::endl;
        std::string peer_uuid = e.peer_uuid();
        BOOST_LOG_TRIVIAL(debug) << " peer id " << peer_uuid << std::endl;
        zmsg_t* msg = e.message();
        handle_whisper(peer_uuid, msg); }
        break;
      case ZYRE_EVENT_SHOUT: {
        std::string group_id = e.group();
        BOOST_LOG_TRIVIAL(debug) << e.peer_name() << " " << group_id 
		   << " shouts ->" << node_name_ << std::endl;
        std::string peer_uuid = e.peer_uuid();
        BOOST_LOG_TRIVIAL(debug) << " peer id " << peer_uuid << std::endl;
        zmsg_t* msg = e.message();
        handle_shout(group_id, peer_uuid, msg); }
        break;
      case ZYRE_EVENT_ENTER: {
        std::string name = e.peer_name();
        BOOST_LOG_TRIVIAL(debug) << name << " enters | " << node_name_ << std::endl; }
        break;
      case ZYRE_EVENT_JOIN: {
        std::string group_id = e.group();
        BOOST_LOG_TRIVIAL(debug) << e.peer_name() << " joins " << group_id << " | " 
		   << node_name_ << std::endl;
        {
          basic_lock lk(join_mutex_);
          joins_[group_id]++; 
        }
        join_cond_.notify_all(); }
        break; 
      case ZYRE_EVENT_LEAVE: {
        std::string group_id = e.group();
        BOOST_LOG_TRIVIAL(debug) << e.peer_name() << " leaves " << group_id << " | " 
		   << node_name_ << std::endl;
	{
	  basic_lock lk(join_mutex_);
	  joins_[group_id]--; 
	}}
        break;
      case ZYRE_EVENT_EXIT: {
	if (e.peer_uuid() == self_->uuid()) {
	  stop();
	}
        BOOST_LOG_TRIVIAL(debug) << e.peer_name() << " exits | " << node_name_ << std::endl; }
        break;
      case ZYRE_EVENT_STOP: {
	if (e.peer_uuid() == self_->uuid()) {
	  stop();
	}
        BOOST_LOG_TRIVIAL(debug) << e.peer_name() << " stops | " << node_name_ << std::endl; }
        return false;
      default:
	BOOST_LOG_TRIVIAL(debug) << "got an unexpected event" << std::endl;
      }
    } else {
      BOOST_LOG_TRIVIAL(debug) << "No event" << std::endl;
      join_cond_.notify_all();
      return false;
    }
    return true;    
  }

  void
  GroupNodeImpl::update_groups()
  {
#ifndef _WIN32
    sigset_t signal_set;
    sigaddset(&signal_set, SIGINT);
    sigaddset(&signal_set, SIGTERM);
    sigaddset(&signal_set, SIGHUP);
    sigaddset(&signal_set, SIGPIPE);
    pthread_sigmask(SIG_BLOCK, &signal_set, NULL);
#endif
    // called by a thread in an infinite loop at a fixed rate (i.e. 1s)    
    while (ok()) {
      auto start = std::chrono::high_resolution_clock::now();
      zlist_t* all_groups = zyre_peer_groups(node_);
      void* item = zlist_first(all_groups);
      while (item != NULL) {
	char* grp = static_cast<char*>(item);
	// zyre only joins the group if we are not ALREADY in the group
	// so it does a hash lookup - we would be duplicating work 
	zyre_join(node_, grp);
	item = zlist_next(all_groups);
      }
      std::chrono::duration<double,std::milli> period(1000);
      std::this_thread::sleep_until(start + period);
    }
  }

  /** \brief Start the listener. This method does not return.
      
      Spinning a group node is required for receiving messages
      whether you plan to access the messages synchronously OR
      asynchronously: something still must listen to the underlying
      network.
  */
  void 
  GroupNodeImpl::spin()
  {
    //event_thread_ = NULL;
    bool continue_spinning = true;
    while (ok() && continue_spinning) {
      continue_spinning = spin_once();
    }
    
  }
  void
  GroupNode::spin()
  {
    self->spin();
  }

  void GroupNodeImpl::_spin()
  {  
    // sigset_t signal_set;
    // sigaddset(&signal_set, SIGINT);
    // sigaddset(&signal_set, SIGTERM);
    // sigaddset(&signal_set, SIGHUP);
    // sigaddset(&signal_set, SIGPIPE);
    // pthread_sigmask(SIG_BLOCK, &signal_set, NULL);
    spin();
  }

  /** \brief Start the listener thread. This method will return immediately.
      Spinning a group node is required for receiving messages
      whether you plan to access the messages synchronously OR
      asynchronously: something still must listen to the underlying
      network.
  */
  void 
  GroupNodeImpl::async_spin()
  {        
    BOOST_LOG_TRIVIAL(debug) << "starting async thread..." << std::endl;
    // start a thread to call the event handlers
    event_thread_ = new std::thread(std::mem_fun(&GroupNodeImpl::_spin), this);
    BOOST_LOG_TRIVIAL(debug) << "started..." << std::endl;
  }
  void
  GroupNode::async_spin()
  {
    self->async_spin();
  }

  void 
  GroupNodeImpl::join()    
  {    
    event_thread_->join();    
  }
  void
  GroupNode::join()
  {
    self->join();
  }
}
