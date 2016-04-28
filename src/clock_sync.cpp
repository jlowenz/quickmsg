#include "clock_sync.hpp"
#include <limits>

namespace pt = boost::posix_time;

inline cs_time_t 
correct_time(const cs_time_t& t)
{
  double offset = SyncClient::offset();  
  bool is_neg = offset < 0;
  uint64_t soff = is_neg ? (uint32_t)std::trunc(-offset) : (uint32_t)std::trunc(offset);
  double dnoff, dint;
  dnoff = std::modf(offset, &dint);
  uint64_t noff = is_neg ? (uint32_t)(-dnoff * 1e9) : (uint32_t)(dnoff * 1e9);
  cs_time_t csoff = (soff << 32) | noff;
  if (is_neg) csoff = ~csoff + 1;
  cs_time_t newt = (t + csoff);
  BOOST_LOG_TRIVIAL(debug) << "offset: " << offset;
  BOOST_LOG_TRIVIAL(debug) << "correcting time " << t << " -> " << newt;
  return newt;
}

int get_time(cs_time_t& ts, bool client = false)
{
  struct timespec time;
  if (clock_gettime(CLOCK_REALTIME, &time)) {
    return -1;
  }
  //BOOST_LOG_TRIVIAL(debug) << sizeof(time.tv_sec) << "," << sizeof(time.tv_nsec);
  //BOOST_LOG_TRIVIAL(debug) << time.tv_sec << "," << time.tv_nsec;
  ts = (time.tv_sec << 32) | time.tv_nsec;
  if (client)
    ts = correct_time(ts);
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
  if (get_time(msg->t2)) {
    msg->valid = 0;
  }
  //BOOST_LOG_TRIVIAL(debug) << "after_recv";
}

void SyncServer::respond(asio::yield_context yield,
			 msg_ptr& msg,
			 udp_sock_ptr sock,
			 udp::endpoint peer) 
{
  if (get_time(msg->t3)) {
    msg->valid = 0;
  }
  sock->send_to(asio::buffer(msg.get(),sizeof(sync_message_t)), peer);
  //msg->valid = msg->id = msg->t1 = msg->t2 = msg->t3 = msg->t4 = 0;
  //delete_msg(msg.get());
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
  : hostname_(hostname), id_(0), deadline_(io_), first_(true)
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
      double offset = get_offset(msg);
      BOOST_LOG_TRIVIAL(debug) << "  offset: " << offset;

      if (first_) {
	offset_.store(offset);
	delay_.store(delay); // hmmm what to do with the delay
	first_ = false;
      } else {
	double o = offset_.load();
	double d = delay_.load();
	//offset_.store(0.9*o + 0.1*(o+offset));
	//offset_.store(0.1*o + 0.9*(o+offset));
	offset_.store(o+offset);
	delay_.store(d);
      }
    } else {
      BOOST_LOG_TRIVIAL(debug) << "Invalid response";
    }
    std::chrono::duration<double, std::milli> period(500);
    std::this_thread::sleep_until(start + period);
  }
}
  
double
SyncClient::get_offset(const sync_message_t::ptr& msg)
{
  double t1 = cs2double2(msg->t1);
  double t2 = cs2double2(msg->t2);
  double t3 = cs2double2(msg->t3);
  double t4 = cs2double2(msg->t4);
  double dd1 = t2 - t1;
  double dd2 = t3 - t4;
  return (dd1 + dd2) / 2.0;
}

double
SyncClient::get_delay(const sync_message_t::ptr& msg)
{
  double t1 = cs2double2(msg->t1);
  double t2 = cs2double2(msg->t2);
  double t3 = cs2double2(msg->t3);
  double t4 = cs2double2(msg->t4);
  double t41 = t4 - t1;
  double t32 = t3 - t2;
  return t41 - t32;
}

void
client_read_handler(SyncClient* self,
		    sync_message_t::ptr& msg,
		    sys::error_code* out_ec,
		    const sys::error_code& ec,
		    size_t num_bytes)
{
  if (get_time(msg->t4,true)) {
    msg->valid = 0;
  }
  *out_ec = ec;
  if (ec) {
    msg->valid = 0;
    BOOST_LOG_TRIVIAL(warning) << ec.message();
  } else {
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
  if (get_time(msg->t1,true)) {
    msg->valid = 0;
  }
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
