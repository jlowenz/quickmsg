#include <boost/log/trivial.hpp>
#include <quickmsg/quickmsg.hpp>
#include <quickmsg/group_node.hpp>
#include <quickmsg/quickmsg_wrap.h>
#include <vector>

using namespace quickmsg;

extern "C" {

  void
  qm_init(const char* name, const char* iface, bool handle_signals)
  {
    std::string sname(name);
    std::string siface(iface);
    init(sname, siface, handle_signals);
    BOOST_LOG_TRIVIAL(info) << "QuickMsg is initializing with name: " << name << " and interface: " << iface << ", handling signals? " << handle_signals << std::endl;
  }

  void
  qm_shutdown(const char* reason)
  {
    shutdown(reason);
  }

  int
  qm_ok()
  {
    return (int)ok();
  }

  double
  qm_get_message_stamp(qm_message_t self_p)
  {
    const Message* msg = reinterpret_cast<const Message*>(self_p);
    return msg->get_stamp();
  }

  const char* 
  qm_get_message_src(qm_message_t self_p)
  {
    const Message* msg = reinterpret_cast<const Message*>(self_p);
    return msg->get_src().c_str();
  }

  const char* 
  qm_get_message_str(qm_message_t self_p)
  {
    const Message* msg = reinterpret_cast<const Message*>(self_p);
    return msg->msg.c_str();
  }

  int
  qm_get_successful(qm_service_reply_t self_p)
  {
    ServiceReply* rep = reinterpret_cast<ServiceReply*>(self_p);
    return (int)rep->successful;
  }

  void 
  qm_message_destroy(qm_message_t self_p)
  {
    const Message* msg = reinterpret_cast<const Message*>(self_p);
    if (msg) delete msg;
  }

  /* this may need to be somewhere else */
  char*
  qm_alloc_string(int length)
  {
    char* out = (char*)malloc(sizeof(char)*(length+1));
    out[length] = '\0';
    return out;
  }

  void 
  qm_free_string(char* str)
  {
    if (str) free(str);
  }

#define VEC_IMPL_OF(CPP_TYPE,C_TYPE) 					\
  size_t qm_vec_##C_TYPE##_size(qm_vec_##C_TYPE o) {			\
    std::vector<PTR(CPP_TYPE)>* v = reinterpret_cast<std::vector<PTR(CPP_TYPE) >*>(o); \
    return v->size(); }							\
  C_TYPE qm_vec_##C_TYPE##_get(qm_vec_##C_TYPE o, size_t elem) {	\
    std::vector<PTR(CPP_TYPE)>* v = reinterpret_cast<std::vector<PTR(CPP_TYPE) >*>(o); \
    return reinterpret_cast<C_TYPE>((*v)[elem]); }			\
  void qm_vec_##C_TYPE##_destroy(qm_vec_##C_TYPE o) {			\
    std::vector<PTR(CPP_TYPE)>* v = reinterpret_cast<std::vector<PTR(CPP_TYPE) >*>(o); \
    size_t SZ = qm_vec_##C_TYPE##_size(o);				\
      for (size_t i = 0; i < SZ; ++i) {					\
	PTR(CPP_TYPE) m = v->operator[](i);				\
	delete m;						        \
      }									\
      delete v; }
  
  VEC_IMPL_OF(quickmsg::Peer,qm_peer_t) // qm_vec_qm_peer_t
  VEC_IMPL_OF(quickmsg::Message,qm_message_t) // qm_vec_qm_message_t

#ifdef _WIN32
  #include <windows.h>
  void msleep(long milliseconds) {
    Sleep(milliseconds);
  }
#else
  #include <time.h>
  void msleep(long milliseconds) {
    struct timespec ts = {0};
    ts.tv_sec = milliseconds / 1000;
    ts.tv_nsec = (milliseconds % 1000) * 1000000;
    nanosleep(&ts, NULL);
  }
#endif
}
