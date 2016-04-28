#ifndef __UDP_SERVER_HPP
#define __UDP_SERVER_HPP

#pragma once

#include <atomic>
#include <boost/asio.hpp>
#include <boost/asio/spawn.hpp>
#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/make_shared.hpp>
#include <boost/log/trivial.hpp>
#include <boost/pool/object_pool.hpp>
#include <boost/thread.hpp>
#include <cstdint>

namespace asio=boost::asio;
namespace sys=boost::system;

using asio::ip::udp;

typedef udp::socket sock_t;
typedef boost::shared_ptr<sock_t> udp_sock_ptr;

template<typename T>
class UDPServer
{
public:
  typedef boost::shared_ptr<T> msg_ptr;

  UDPServer(asio::io_service& service, uint32_t port);
  virtual ~UDPServer();

  virtual void before_recv(msg_ptr& msg) {}
  virtual void after_recv(msg_ptr& msg) {}
  virtual void respond(asio::yield_context yield,
		       msg_ptr& msg,
		       udp_sock_ptr sock,
		       udp::endpoint peer) = 0;

  void run(asio::yield_context yield);
private:
  T* new_msg();
  void delete_msg(T* msg);
  
  asio::io_service& io_;
  uint32_t port_;
  udp_sock_ptr sock_;
  std::atomic<bool> ok_;
  boost::object_pool<T> pool_;
  boost::mutex pool_mutex_;
};

template<typename T>
UDPServer<T>::UDPServer(asio::io_service& service, uint32_t port)
  : io_(service), port_(port)
{
  sock_ = boost::make_shared<sock_t>(service, 
				     udp::endpoint(udp::v4(),port_));
  ok_.store(true);  
}

template<typename T>
void UDPServer<T>::run(asio::yield_context yield) {
  udp::endpoint remote_peer;
  sys::error_code ec;
  while (ok_.load()) {
    msg_ptr msg(new_msg(), [](T*){});
    before_recv(msg);
    sock_->async_receive_from(asio::buffer(msg.get(),
					   sizeof(T)),
			      remote_peer, yield[ec]);
    after_recv(msg);
    if (!ec) {
      asio::spawn(io_, boost::bind(&UDPServer::respond, this, ::_1, 
				   msg, sock_, remote_peer));
    }
  }
}

template<typename T>
UDPServer<T>::~UDPServer()
{
}

template<typename T>
T* UDPServer<T>::new_msg()
{
  boost::mutex::scoped_lock lock(pool_mutex_);
  return pool_.construct();
}

template<typename T>
void UDPServer<T>::delete_msg(T* msg) {
  boost::mutex::scoped_lock lock(pool_mutex_);
  pool_.free(msg);
}


#endif
