#include "clock_sync.hpp"
#include "udp_server.hpp"
#include <time.h>
#include <thread>
#include <chrono>
#include <string>

const uint32_t SYNC_PORT = 41337;
const uint32_t NUM_THREADS = 4;

typedef int64_t cs_time_t;

struct sync_message_t
{
  typedef std::shared_ptr<sync_message_t> ptr;
  uint32_t id;
  cs_time_t t1;
  cs_time_t t2;
  cs_time_t t3;
  cs_time_t t4;
  sync_message_t(uint32_t i) : 
    id(i), t1(0), t2(0), t3(0), t4(0) {}
};

int get_time(cs_time_t& ts)
{
  struct timespec time;
  if (clock_gettime(CLOCK_REALTIME, &time)) {
    return -1;
  }
  ts = (time.tv_sec << 32) | time.tv_nsec;
}

class SyncServer : public UDPServer<sync_message_t>
{
public:
  using UDPServer::msg_ptr;

  SyncServer(asio::io_service& service)
    : UDPServer(service, SYNC_PORT)
  {
  }
  ~SyncServer() {}

  void after_recv(msg_ptr& msg) 
  {
    get_time(msg->t2);
  }

  void respond(asio::yield_context yield,
	       msg_ptr& msg,
	       udp_sock_ptr sock,
	       udp::endpoint peer) 
  {
    get_time(msg->t3);
    sock->send_to(asio::buffer(msg.get(),sizeof(sync_message_t)), peer);
  }
};

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

class SyncClient
{
public:

  SyncClient(std::string hostname)
    : hostname_(hostname)
  {
    udp::resolver::query q(udp::v4(), hostname_, std::to_string(SYNC_PORT));
    udp::resolver resolver(io_);
    auto iter = resolver.resolve(q);
    svc_ = iter->endpoint();
    sock_.reset(new udp::socket(io_, udp::v4()));
  }
  ~SyncClient() {}

  void run() {
    ok_.store(true);
    while (ok_.load()) {
      auto start = std::chrono::high_resolution_clock::now();
      sync_message_t::ptr msg = req_sync_message();
      BOOST_LOG_TRIVIAL(debug) << "Server queried:" << std::endl;
      BOOST_LOG_TRIVIAL(debug) << "      t1: " << msg->t1 << std::endl;
      BOOST_LOG_TRIVIAL(debug) << "      t2: " << msg->t2 << std::endl;
      BOOST_LOG_TRIVIAL(debug) << "      t3: " << msg->t3 << std::endl;
      BOOST_LOG_TRIVIAL(debug) << "      t4: " << msg->t4 << std::endl;
      BOOST_LOG_TRIVIAL(debug) << "  offset: " << get_offset(msg) << std::endl;
      BOOST_LOG_TRIVIAL(debug) << "   delay: " << get_delay(msg) << std::endl;
      std::chrono::duration<double, std::milli> period(500);
      std::this_thread::sleep_until(start + period);
    }
  }
  
protected:
  int64_t get_offset(const sync_message_t::ptr& msg)
  {
    return ((msg->t2 - msg->t1) + (msg->t3 - msg->t4)) / 2;
  }
  
  int64_t get_delay(const sync_message_t::ptr& msg)
  {
    return (msg->t4 - msg->t1) - (msg->t3 - msg->t2);
  }
  
  sync_message_t::ptr req_sync_message()
  {
    sync_message_t::ptr msg(new sync_message_t());
    // what about return values?
    get_time(msg->t1);
    sock_.send_to(asio::buffer(msg.get(), 
			       sizeof(sync_message_t)), 
		  svc_);
    size_t recvd = sock_.receive_from(asio::buffer(msg.get(), 
						   sizeof(sync_message_t)), 
				      svc_);
    get_time(msg->t4);
    return msg;
  }

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

std::atomic<int32_t> SyncClient::offset_(0);
std::atomic<int32_t> SyncClient::delay_(0);
