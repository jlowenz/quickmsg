#ifndef __CLOCK_SYNC_HPP
#define __CLOCK_SYNC_HPP

#pragma once

#include "udp_server.hpp"

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

typedef int64_t cs_time_t;

inline double cs2double(const cs_time_t& t)
{
  bool is_neg = t < 0;
  cs_time_t cs = is_neg ? (~t + 1) : t;
  double d = ldexp((double)cs, -32);
  return is_neg ? -d : d;
}

struct sync_message_t
{
  typedef std::shared_ptr<sync_message_t> ptr;
  uint32_t id;
  cs_time_t t1;
  cs_time_t t2;
  cs_time_t t3;
  cs_time_t t4;

  sync_message_t() : 
    id(0), t1(0), t2(0), t3(0), t4(0) {}
  sync_message_t(uint32_t i) : 
    id(i), t1(0), t2(0), t3(0), t4(0) {}
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
  
protected:
  double get_offset(const sync_message_t::ptr& msg);
  double get_delay(const sync_message_t::ptr& msg);
  sync_message_t::ptr req_sync_message();

private:
  std::string hostname_;
  std::atomic<bool> ok_;
  std::atomic<int> id_;
  asio::io_service io_;
  udp::endpoint svc_;
  udp_sock_ptr sock_;
  static std::atomic<int32_t> offset_; // in microseconds
  static std::atomic<int32_t> delay_;  // in microseconds
};

#endif
