#include "clock_sync.hpp"
#include <time.h>
#include <thread>
#include <chrono>
#include <string>

namespace pt = boost::posix_time;

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
  srand(time(NULL));
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
  msg->valid = msg->id = msg->t1 = msg->t2 = msg->t3 = msg->t4 = 0;
  delete_msg(msg.get());
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
  : hostname_(hostname), id_(0), deadline_(io_)
{
  udp::resolver::query q(udp::v4(), hostname_, std::to_string(SYNC_PORT));
  udp::resolver resolver(io_);
  auto iter = resolver.resolve(q);
  svc_ = iter->endpoint();
  sock_.reset(new udp::socket(io_, udp::v4()));
  deadline_.expires_at(boost::posix_time::pos_infin);
  check_deadline();
}

void 
SyncClient::check_deadline()
{
  if (deadline_.expires_at() <= asio::deadline_timer::traits_type::now()) {
    // cancel may have portability issues on Windows!
    // TODO: look into this
    sock_->cancel();
    // do nothing until another deadline is set
    deadline_.expires_at(boost::posix_time::pos_infin);
  }
  // Put the actor back to sleep.
  deadline_.async_wait(boost::bind(&SyncClient::check_deadline, this));
}

void 
SyncClient::run() {
  ok_.store(true);
  while (ok_.load()) {
    auto start = std::chrono::high_resolution_clock::now();
    sys::error_code ec = asio::error::would_block;
    sync_message_t::ptr msg = req_sync_message(ec);
    if (msg->valid != 0) {
      BOOST_LOG_TRIVIAL(debug) << "Server queried:";
      BOOST_LOG_TRIVIAL(debug) << "      t1: " << msg->t1;
      BOOST_LOG_TRIVIAL(debug) << "      t2: " << msg->t2;
      BOOST_LOG_TRIVIAL(debug) << "      t3: " << msg->t3;
      BOOST_LOG_TRIVIAL(debug) << "      t4: " << msg->t4;
      double delay = get_delay(msg);
      BOOST_LOG_TRIVIAL(debug) << "   delay: " << delay;
      BOOST_LOG_TRIVIAL(debug) << "  offset: " << get_offset(msg, delay);
    } else {
      BOOST_LOG_TRIVIAL(debug) << "Invalid response";
    }
    std::chrono::duration<double, std::milli> period(500);
    std::this_thread::sleep_until(start + period);
  }
}
  
double
SyncClient::get_offset(const sync_message_t::ptr& msg, double delay)
{
  cs_time_t d1 = (msg->t2 - msg->t1);
  cs_time_t d2 = (msg->t3 - msg->t4);
  double t21 = cs2double(d1);
  double t34 = cs2double(d2);
  return (t21 + t34 - delay) / 2.0;
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

void
client_read_handler(SyncClient* self,
		    sync_message_t::ptr& msg,
		    sys::error_code* out_ec,
		    const sys::error_code& ec,
		    size_t num_bytes)
{
  get_time(msg->t4);
  *out_ec = ec;
  if (ec) {
    msg->valid = 0;
    BOOST_LOG_TRIVIAL(warning) << ec.message();
  } else {
    BOOST_LOG_TRIVIAL(debug) << "client_read_handler";
    msg->valid = 1;
  }
}
  
std::ostream& operator<<(std::ostream& out, const sync_message_t::ptr& sm)
{
  out << "SyncMessage[" << sm->valid << "," << sm->id << "," << sm->t1 << "," << sm->t2 << "," <<
    sm->t3 << "," << sm->t4 << "]";
  return out;
}

sync_message_t::ptr 
SyncClient::req_sync_message(sys::error_code& ec)
{
  pt::time_duration timeout = pt::milliseconds(REQUEST_TIMEOUT);
  uint32_t id = id_++;
  sync_message_t::ptr msg(new sync_message_t(id));
  // what about return values?
  get_time(msg->t1);
  sock_->send_to(asio::buffer(msg.get(), 
			      sizeof(sync_message_t)), svc_);

  // start the timer
  deadline_.expires_from_now(timeout);
  // receive
  auto buf = asio::buffer(msg.get(), sizeof(sync_message_t));
  sock_->async_receive_from(buf, svc_, 
			    boost::bind(client_read_handler, this,
					msg, &ec, ::_1, ::_2));
  // block
  do io_.run_one(); while (ec == asio::error::would_block);
  if (id != msg->id) {
    BOOST_LOG_TRIVIAL(warning) << "message id out of sync";
    msg->valid = 0;
  }
  BOOST_LOG_TRIVIAL(debug) << msg;
  return msg;
}

std::atomic<double> SyncClient::offset_(0);
std::atomic<double> SyncClient::delay_(0);

double
SyncClient::offset()
{
  return SyncClient::offset_.load();
}

double
SyncClient::delay()
{
  return SyncClient::delay_.load();
}
