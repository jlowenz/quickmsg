#ifndef __SUBSCRIBER_WRAP_H_INCLUDED__
#define __SUBSCRIBER_WRAP_H_INCLUDED__

#include <quickmsg/ctypes.h>

#ifdef __cplusplus
extern "C" {
#endif

  qm_subscriber_t
  qm_subscriber_new (const char* topic, int queue_sz);

  qm_vec_qm_message_t 
  qm_subscriber_get_messages(qm_subscriber_t self_p);

  void
  qm_subscriber_destroy (qm_subscriber_t self_p);
  
  qm_async_subscriber_t
  qm_async_subscriber_new(const char* topic, qm_message_handler_t handler, void* args);

  void
  qm_async_subscriber_spin(qm_async_subscriber_t self);

  // TODO: should these be pointers - so the function can set the var to NULL?
  void
  qm_async_subscriber_destroy(qm_async_subscriber_t self_p);
  

#ifdef __cplusplus
}
#endif

#endif
