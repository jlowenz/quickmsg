#include <quickmsg/types.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#define MILLION 1000000

namespace quickmsg {
  using namespace boost::posix_time;

  Time::Time()
    : secs_(0), msecs_(0)
  {
  }
  Time::Time(double time)
  {
    secs_=static_cast<uint32_t>(std::floor(time));
    msecs_=static_cast<uint32_t>(std::floor(1e6*(time - std::floor(time))));
  }
  Time::Time(uint32_t secs, uint32_t micro)
    : secs_(secs), msecs_(micro)
  {
  }
  Time::Time(const std::chrono::high_resolution_clock::time_point& t)
  {
    std::chrono::high_resolution_clock::duration d = t.time_since_epoch();
    secs_ = static_cast<uint64_t>(std::chrono::duration_cast<std::chrono::seconds>(d).count());
    uint64_t micros = static_cast<uint64_t>(std::chrono::duration_cast<std::chrono::microseconds>(d).count());
    uint64_t secs = MILLION*secs_;
    msecs_ = micros - secs;
  }
  Time::Time(const Time& t)
  {
    secs_=t.secs();
    msecs_=t.microsecs();
  }
  Time::~Time()
  {
  }

  Time& 
  Time::operator=(const Time& t)
  {
    secs_=t.secs();
    msecs_=t.microsecs();
    return *this;
  }

  bool 
  Time::operator==(const Time& t) const
  {
    return (secs_==t.secs() && msecs_==t.microsecs());
  }

  bool 
  Time::operator<(const Time& t) const
  {
    if (secs_==t.secs())
    {
      return (msecs_ < t.microsecs());
    }
    else return (secs_ < t.secs());
  }

  double 
  Time::to_secs() const
  {
    return static_cast<double>((secs_+(static_cast<double>(msecs_)/1e6)));
  }

  uint64_t 
  Time::secs() const
  {
    return secs_;
  }

  uint64_t 
  Time::microsecs() const
  {
    return msecs_;
  }

  std::string 
  Time::to_string() const
  {
    return "";
  }

  std::string 
  Time::to_json() const
  {
    return "";
  }

  Time time_now()
  {
    Time t(std::chrono::high_resolution_clock::now());
    return t;
  }

  Message::Message(const Message& m)
    : header(m.header), msg(m.msg)
  {    
  }

  double 
  Message::get_stamp() const
  {
    return header.stamp.to_secs();
  }

  std::string
  Message::get_context() const
  {
    return header.context;
  }

  std::string
  Message::get_src() const
  {
    return header.src_uuid;
  }

  std::string
  Message::get_msg() const 
  {
    return msg;
  }

  void
  Message::set_stamp_now()
  {
    header.stamp=time_now();
  }

  void
  Message::set_context(const std::string& ctx)
  {
    header.context=ctx;
  }

  void
  Message::set_msg(const std::string& msg_str)
  {
    msg=msg_str;
  }

  ServiceReply::ServiceReply(const ServiceReply& r)
    : Message(r), successful(r.successful)
  {
  }

  ServiceReply::ServiceReply(const Message& r, bool success)
    : Message(r), successful(success)
  {
  }


  std::ostream& operator<<(std::ostream& os, const Time& t)
  {
    os << t.secs() << "." << t.microsecs();
    return os;
  }

  std::ostream& operator<<(std::ostream& os, const Message& m)
  {
    os << "src: " << m.header.src_uuid << std::endl;
    os << "stamp: " << m.header.stamp << std::endl;
    os << "msg: " << m.msg << std::endl;
    return os;
  }
}
