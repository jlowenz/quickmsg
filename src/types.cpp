#include <quickmsg/types.hpp>

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
  Time::Time(const ptime& t)
  {
    time_duration d = t - from_time_t(std::time_t(0));
    secs_ = d.total_seconds();
    uint64_t msecs = d.total_microseconds();
    uint64_t secs = static_cast<uint64_t>(1e6*d.total_seconds());
    msecs_ = static_cast<uint32_t>(msecs-secs);
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

  uint32_t 
  Time::secs() const
  {
    return secs_;
  }

  uint32_t 
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
    Time t(microsec_clock::universal_time());
    return t;
  }

  double 
  Message::get_stamp()
  {
    return header.stamp.to_secs();
  }

  std::string
  Message::get_context()
  {
    return header.context;
  }

  std::string
  Message::get_src()
  {
    return header.src_uuid;
  }

  std::string
  Message::get_msg()
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
