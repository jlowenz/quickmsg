#include <boost/log/trivial.hpp>
#include <quickmsg/publisher.hpp>
#include <quickmsg/group_node.hpp>

namespace quickmsg {

  Publisher::Publisher(const std::string& topic, bool wait)
    : topic_(topic)
  {
    // create the group node
    std::string name("P/");
    node_ = new GroupNode(name + topic);
    node_->join(topic_);
    node_->async_spin();
    BOOST_LOG_TRIVIAL(info) << "Waiting for a subscriber on topic "<<topic<<std::endl;
    if (wait) node_->wait_join(topic_);
  }

  Publisher::~Publisher()
  {
    node_->leave(topic_);
    node_->stop();
    delete node_;
  }

  void 
  Publisher::join()
  {
    node_->join();
  }

  void 
  Publisher::publish(const std::string& msg)
  {
    node_->shout(topic_, msg); // done
  }

}
