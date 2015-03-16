#include <quickmsg/types.hpp>

#include <boost/date_time/posix_time/posix_time.hpp>

namespace quickmsg {
  Time::Time()
    : secs_(0), msecs_(0)
  {
  }
  Time::Time(double time)
  {
  }
  Time::Time(uint32_t secs, uint32_t micro)
  {
  }
  Time::Time(const Time& t)
  {
  }
  Time::~Time()
  {
  }

  Time& 
  Time::operator=(const Time& t)
  {
    return *this;
  }

  bool 
  Time::operator==(const Time& t) const
  {
    return false;
  }

  bool 
  Time::operator<(const Time& t) const
  {
    return false;
  }

  double 
  Time::to_secs() const
  {
    return 0.0;
  }

  uint32_t 
  Time::secs() const
  {
    return 0;
  }

  uint32_t 
  Time::microsecs() const
  {
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
}
