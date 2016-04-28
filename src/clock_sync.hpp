#ifndef __CLOCK_SYNC_HPP
#define __CLOCK_SYNC_HPP

#pragma once

#include "udp_server.hpp"
#include <cmath>
#include <boost/asio/deadline_timer.hpp>
#include <time.h>
#include <thread>
#include <chrono>
#include <string>

class ClockSync
{
public:
  ClockSync();
  virtual ~ClockSync();
  
  void run_as_service();
  void run_as_client();
private:
};

const uint32_t SYNC_PORT = 41337;
const uint32_t NUM_THREADS = 4;
const uint32_t REQUEST_TIMEOUT = 400;

typedef uint64_t cs_time_t;
//typedef timespec cs_time_t;


inline double cs2double(const cs_time_t& t)
{ 
  bool is_neg = t < 0;
  cs_time_t cs = is_neg ? (~t + 1) : t;
  double d = ldexp((double)cs, -32);
  return is_neg ? -d : d;
}

inline double cs2double2(const cs_time_t& t)
{
  uint32_t s = t >> 32;
  uint32_t n = (uint32_t)(t & 0xffffffff);
  return (double)(s + n / 1.0e9);
}

struct sync_message_t
{
  typedef std::shared_ptr<sync_message_t> ptr;
  uint32_t id;
  cs_time_t t1;
  cs_time_t t2;
  cs_time_t t3;
  cs_time_t t4;
  uint32_t valid;

  sync_message_t() : 
    id(0), t1(0), t2(0), t3(0), t4(0), valid(0) {}
  sync_message_t(uint32_t i) : 
    id(i), t1(0), t2(0), t3(0), t4(0), valid(0) {}
};

class SyncServer : public UDPServer<sync_message_t>
{
public:
  using UDPServer::msg_ptr;

  SyncServer(asio::io_service& service);
  ~SyncServer();

  void after_recv(msg_ptr& msg);
  void respond(asio::yield_context yield,
	       msg_ptr& msg,
	       udp_sock_ptr sock,
	       udp::endpoint peer);
};

void run_sync_service();

class SyncClient
{
public:

  SyncClient(std::string hostname);
  ~SyncClient() {}
  void run();
  
  // Offset and delta in seconds.
  static double offset();
  static double delay();

protected:
  void check_deadline();
  double get_offset(const sync_message_t::ptr& msg);
  double get_delay(const sync_message_t::ptr& msg);
  sync_message_t::ptr req_sync_message(sys::error_code& ec);

private:
  asio::io_service io_;
  std::string hostname_;
  std::atomic<bool> ok_;
  int id_;
  udp::endpoint svc_;
  udp_sock_ptr sock_;
  asio::deadline_timer deadline_;
  bool first_;
  
  // for 
  static std::atomic<double> offset_; // in microseconds
  static std::atomic<double> delay_;  // in microseconds
};

#endif
