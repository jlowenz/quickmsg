#pragma once

#include <string>
//#include <list>
#include <vector>
#include <memory>
#include <chrono>

namespace quickmsg {

#if !SWIG
  // TODO: switch to std::chrono instead?
  class Time 
  {
  public:
    Time();
    explicit Time(double time);
    Time(uint32_t secs, uint32_t micro);
    Time(const Time& t);
    // removed ptime constructor
    Time(const std::chrono::high_resolution_clock::time_point& tp);
    ~Time();

    Time& operator=(const Time& t);
    bool operator==(const Time& t) const;
    bool operator<(const Time& t) const;

    double to_secs() const;
    uint64_t secs() const;
    uint64_t microsecs() const;
    std::string to_string() const;
    std::string to_json() const;
    
  private:
    uint64_t secs_;
    uint64_t msecs_;
  };

  Time time_now();
  
  class Header
  {
  public:
    Time stamp;
    std::string context;
    std::string src_uuid;

    Header() {}
    friend std::ostream& operator<<(std::ostream& os, const Time& t);
  };
#endif

  class Message
  {
  public:
    Header header;
    std::string msg;

    Message() {}
    Message(const Message& m);
    virtual ~Message() {}

    double get_stamp() const;
    std::string get_context() const;
    std::string get_src() const;
    std::string get_msg() const;

    void set_stamp_now();
    void set_context(const std::string& ctx);
    void set_msg(const std::string& msg_str);
    friend std::ostream& operator<<(std::ostream& os, const Message& m);
  };


  class ServiceReply : public Message
  {
  public:
    ServiceReply() {}    
    ServiceReply(const ServiceReply& s);
    ServiceReply(const Message& r, bool success = true);
    virtual ~ServiceReply() {}
    bool successful;
  };

  // shared pointers
  typedef std::shared_ptr<Message> MessagePtr;
  typedef std::shared_ptr<ServiceReply> ServiceReplyPtr;

  typedef std::vector<Message*> MsgList;

  // common message callback type
  // Message is owned by the caller, so copy any data needed
  typedef void (*MessageCallback)(const Message*,void*); 

  // service handler callback
  typedef char* (*ServiceCallback)(const Message*,void*);
}
