#ifndef __CLOCK_SYNC_HPP
#define __CLOCK_SYNC_HPP

#pragma once

class ClockSync
{
public:
  ClockSync();
  virtual ~ClockSync();
  
  void run_as_service();
  void run_as_client();
private:
};

#endif
