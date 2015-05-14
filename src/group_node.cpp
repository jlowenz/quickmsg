#include <quickmsg/group_node.hpp>
#include <iterator> 
#include <thread>
#include <functional>
#include <algorithm>

namespace quickmsg {

  // static_assert(std::is_trivially_copyable<std::string>::value, 
  // 		"std::atomic<std::string> requires std::string to be trivially copyable");

  // we will not protect name_ since it will be set once, and otherwise remain read-only
  std::string GroupNode::name_("");
  std::atomic_bool GroupNode::running_(false);

  std::string GroupNode::name()
  {
    return GroupNode::name_;
  }

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

  GroupNode::GroupNode(const std::string& desc, bool promiscuous)
    : promiscuous_(promiscuous)
  {
    // create the zyre node
    node_ = zyre_new((GroupNode::name() + "/" + desc).c_str());
    // set the headers
    zyre_set_header(node_, "desc", "%s", desc.c_str());
    // access our uuid
    std::string uuid = zyre_uuid(node_);
    // access our name
    node_name_ = zyre_name(node_);
    // start the node
    if (zyre_start(node_)) {
      throw std::runtime_error("Could not start the zyre node");
    }
    GroupNode::running_ = true;
    // create our self peer
    self_.reset(new Peer(uuid, desc));
  }
  
  GroupNode::~GroupNode()
  {    
    zyre_destroy(&node_);
  }
    
  void    
  GroupNode::join(const std::string& group)
  {
    if (zyre_join(node_, group.c_str())) {
      throw std::runtime_error("Error joining group: " + group);
    }    
  }

  void
  GroupNode::wait_join(const std::string& group)
  {
    // how do we wait for joins? we should receive at least one join message
    // but we MAY have received it in the PAST! 
    basic_lock lk(join_mutex_); 
    if (joins_[group] > 0) return;
    else {
      join_cond_.wait(lk, [&](){ return joins_[group] > 0; });
    }
  }
  
  void
  GroupNode::leave(const std::string& group)
  {
    if (zyre_leave(node_, group.c_str())) {
      throw std::runtime_error("Error leaving group: " + group);
    }
  }
    
  void 
  GroupNode::register_handler(const std::string& group, MessageCallback cb, void* args)
  {
    // add to the handlers
    handlers_.insert(std::make_pair(group,std::make_pair(cb, args)));
  }

  void 
  GroupNode::register_whispers(MessageCallback cb, void* args)
  {
    // add to the handlers
    whisper_handler_ = std::make_pair(cb,args);
  }


  void 
  GroupNode::shout(const std::string& group, const std::string& msg)
  {
    //zmsg_t* zmsg = zmsg_new();
    //zmsg_pushstr(zmsg, msg.c_str());
    if (zyre_shouts(node_, group.c_str(), "%s", msg.c_str())) {
      throw std::runtime_error("Error sending message to topic: " + group);
    }
  }

  void 
  GroupNode::whisper(const std::string& peer_uuid, const std::string& msg)
  {
    if (zyre_whispers(node_, peer_uuid.c_str(), "%s", msg.c_str())) {
      throw std::runtime_error("Error sending message to peer: " + peer_uuid);
    }
  }

  void 
  GroupNode::whisper(const PeerPtr& peer, const std::string& msg)
  {
    if (zyre_whispers(node_, peer->uuid().c_str(), "%s", msg.c_str())) {
      throw std::runtime_error("Error sending message to peer: " + peer->uuid());
    }
  }

  void
  GroupNode::peers(PeerList& ps) const
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
  GroupNode::peers_by_description(PeerList& ps, const std::string& desc) const
  {
    PeerList tmp_ps;
    this->peers(tmp_ps);
    PeerList::iterator new_end = std::remove_if(tmp_ps.begin(), tmp_ps.end(), [&](PeerPtr& p) {
	return p->uuid() != std::string(desc); });
    std::copy(tmp_ps.begin(), new_end, std::back_inserter(ps));
  }
  
  /** \brief Terminate any processing for this node.
      Terminate any processing for this node, non-recoverable (for now).
  */
  void 
  GroupNode::stop()
  {
    zyre_stop(node_);
  }

  void
  GroupNode::handle_whisper(const std::string& uuid, zmsg_t* zmsg)
  {    
    MessagePtr msg(new Message);
    msg->header.stamp = time_now();
    msg->header.context = uuid;
    msg->header.src_uuid = uuid;
    char* a_str = zmsg_popstr(zmsg);
    msg->msg = a_str;
    free(a_str);
    whisper_handler_.first(msg, whisper_handler_.second);
  }

  void 
  GroupNode::handle_shout(const std::string& group, const std::string& uuid, zmsg_t* zmsg)
  {
    MessagePtr msg(new Message);
    msg->header.stamp = time_now();
    msg->header.context = group;
    msg->header.src_uuid = uuid;
    char* a_str = zmsg_popstr(zmsg);
    msg->msg = a_str;
    free(a_str);
    //zmsg_destroy(&zmsg);
    auto range = handlers_.equal_range(group);
    std::for_each(range.first,range.second,
		  [&](handlers_t::value_type& x){x.second.first(msg, x.second.second);});

    // handle promiscuity
    if (promiscuous_ && range.first != range.second) {
      range = handlers_.equal_range("*");
      std::for_each(range.first, range.second,
		    [&](handlers_t::value_type& x){x.second.first(msg, x.second.second);});
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
      zyre_event_destroy(&e_);
    }

    bool valid() const {
      return e_ != NULL;
    }

    zyre_event_type_t type() const {
      return zyre_event_type(e_);
    }

    std::string peer_uuid() const {
      return std::string(zyre_event_sender(e_));
    }

    std::string peer_name() const {
      return std::string(zyre_event_name(e_));
    }
    
    std::string group() const {
      return std::string(zyre_event_group(e_));
    }
    
    zmsg_t* message() const {
      return zyre_event_msg(e_);
    }

  private:
    zyre_event_t* e_;
  };

  bool 
  GroupNode::spin_once()
  {
    // read a new event from the zyre node, interrupt
    if (zsys_interrupted) return false;

    ScopedEvent e(zyre_event_new(node_)); // apparently, blocks until event occurs.
    // will be destroyed at the end of the function
    if (e.valid()) {
      zyre_event_type_t t = e.type();
      switch (t) {
      case ZYRE_EVENT_WHISPER: {
        std::string name = e.peer_name();
        std::cerr << name << " whispers -> " << node_name_ << std::endl;
        std::string peer_uuid = e.peer_uuid();
        std::cerr << " peer id " << peer_uuid << std::endl;
        zmsg_t* msg = e.message();
        handle_whisper(peer_uuid, msg); }
        break;
      case ZYRE_EVENT_SHOUT: {
        std::string group_id = e.group();
        std::cerr << e.peer_name() << " " << group_id 
        	  << " shouts ->" << node_name_ << std::endl;
        std::string peer_uuid = e.peer_uuid();
        std::cerr << " peer id " << peer_uuid << std::endl;
        zmsg_t* msg = e.message();
        handle_shout(group_id, peer_uuid, msg); }
        break;
      case ZYRE_EVENT_ENTER: {
        std::string name = e.peer_name();
        std::cerr << name << " enters | " << node_name_ << std::endl; }
        break;
      case ZYRE_EVENT_JOIN: {
        std::string group_id = e.group();
        std::cerr << e.peer_name() << " joins " << group_id << " | " 
        	  << node_name_ << std::endl;
        {
          basic_lock lk(join_mutex_);
          joins_[group_id]++; 
        }
        join_cond_.notify_all(); }
        break; 
      case ZYRE_EVENT_LEAVE: {
        std::string group_id = e.group();
        std::cerr << e.peer_name() << " leaves " << group_id << " | " 
        	  << node_name_ << std::endl;
        joins_[group_id]--; }
        break;
      case ZYRE_EVENT_EXIT: {
        std::cerr << e.peer_name() << " exits | " << node_name_ << std::endl; }
        break;
      case ZYRE_EVENT_STOP: {
        std::cerr << e.peer_name() << " stops | " << node_name_ << std::endl; }
        return false;
      }
    } else {
      return false;
    }
    return true;
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
    event_thread_ = NULL;
    bool continue_spinning = true;
    while (!zsys_interrupted && GroupNode::running_.load() && continue_spinning) {
      continue_spinning = spin_once();
    }
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
    // start a thread to call the event handlers
    event_thread_ = new std::thread(std::mem_fun(&GroupNode::spin), this);
    //    event_thread_->detach();
  }

  void 
  GroupNode::join()    
  {    
    event_thread_->join();
  }
}
