#include "clock_sync.hpp"
#include <time.h>
#include <thread>
#include <chrono>
#include <string>


int get_time(cs_time_t& ts)
{
  struct timespec time;
  if (clock_gettime(CLOCK_REALTIME, &time)) {
    return -1;
  }
  ts = (time.tv_sec << 32) | time.tv_nsec;
  return 0;
}


SyncServer::SyncServer(asio::io_service& service)
  : UDPServer(service, SYNC_PORT)
{
}
SyncServer::~SyncServer() {}

void 
SyncServer::after_recv(msg_ptr& msg) 
{
  get_time(msg->t2);
}

void SyncServer::respond(asio::yield_context yield,
			 msg_ptr& msg,
			 udp_sock_ptr sock,
			 udp::endpoint peer) 
{
  get_time(msg->t3);
  sock->send_to(asio::buffer(msg.get(),sizeof(sync_message_t)), peer);
}

void run_sync_service()
{
  asio::io_service io;
  SyncServer server(io);
  for (uint32_t i = 0; i < NUM_THREADS; ++i) {
    asio::spawn(io, boost::bind(&SyncServer::run, boost::ref(server), ::_1));
  }
  boost::thread_group pool;
  for (uint32_t i = 0; i < NUM_THREADS; ++i) {
    pool.create_thread([&io] { io.run(); });
  }
  pool.join_all();
}

SyncClient::SyncClient(std::string hostname)
  : hostname_(hostname)
{
  udp::resolver::query q(udp::v4(), hostname_, std::to_string(SYNC_PORT));
  udp::resolver resolver(io_);
  auto iter = resolver.resolve(q);
  svc_ = iter->endpoint();
  sock_.reset(new udp::socket(io_, udp::v4()));
}

void 
SyncClient::run() {
  ok_.store(true);
  while (ok_.load()) {
    auto start = std::chrono::high_resolution_clock::now();
    sync_message_t::ptr msg = req_sync_message();
    BOOST_LOG_TRIVIAL(debug) << "Server queried:";
    BOOST_LOG_TRIVIAL(debug) << "      t1: " << msg->t1;
    BOOST_LOG_TRIVIAL(debug) << "      t2: " << msg->t2;
    BOOST_LOG_TRIVIAL(debug) << "      t3: " << msg->t3;
    BOOST_LOG_TRIVIAL(debug) << "      t4: " << msg->t4;
    BOOST_LOG_TRIVIAL(debug) << "  offset: " << get_offset(msg);
    BOOST_LOG_TRIVIAL(debug) << "   delay: " << get_delay(msg);
    std::chrono::duration<double, std::milli> period(500);
    std::this_thread::sleep_until(start + period);
  }
}
  
double
SyncClient::get_offset(const sync_message_t::ptr& msg)
{
  cs_time_t d1 = (msg->t2 - msg->t1);
  cs_time_t d2 = (msg->t3 - msg->t4);
  double t21 = cs2double(d1);
  double t34 = cs2double(d2);
  return (t21 + t34) / 2.0;
}

double
SyncClient::get_delay(const sync_message_t::ptr& msg)
{
  cs_time_t d1 = (msg->t4 - msg->t1);
  cs_time_t d2 = (msg->t3 - msg->t2);
  double t41 = cs2double(d1);
  double t32 = cs2double(d2);
  return t41 - t32;
}
  
sync_message_t::ptr 
SyncClient::req_sync_message()
{
  uint32_t id = id_++;
  sync_message_t::ptr msg(new sync_message_t(id));
  // what about return values?
  get_time(msg->t1);
  sock_->send_to(asio::buffer(msg.get(), 
			      sizeof(sync_message_t)), svc_);
  sock_->receive_from(asio::buffer(msg.get(), 
				   sizeof(sync_message_t)), svc_);
  get_time(msg->t4);
  if (id != msg->id) {
    std::cerr << "bogus message" << std::endl;
  }
  return msg;
}

std::atomic<int32_t> SyncClient::offset_(0);
std::atomic<int32_t> SyncClient::delay_(0);
