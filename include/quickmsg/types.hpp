#pragma once

#include <string>
#include <list>
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>

namespace quickmsg {

  class Time 
  {
  public:
    Time();
    explicit Time(double time);
    Time(uint32_t secs, uint32_t micro);
    Time(const Time& t);
    ~Time();

    Time& operator=(const Time& t);
    bool operator==(const Time& t) const;
    bool operator<(const Time& t) const;

    double to_secs() const;
    uint32_t secs() const;
    uint32_t microsecs() const;
    std::string to_string() const;
    std::string to_json() const;
    
  private:
    uint32_t secs_;
    uint32_t msecs_;
  };

  Time time_now();
  
  class Header
  {
  public:
    Time stamp;
    std::string context;
    std::string src_uuid;

    Header() {}
  };

  class Message
  {
  public:
    Header header;
    std::string msg;

    Message() {}
  };
  typedef boost::shared_ptr<Message> MessagePtr;


  class ServiceReply : public Message
  {
  public:
    ServiceReply() {}    
    bool successful;
  };
  typedef boost::shared_ptr<ServiceReply> ServiceReplyPtr;

  typedef std::list<MessagePtr> MsgList;
  typedef boost::shared_ptr<MsgList> MsgListPtr;
  
}
