#ifndef __QUICKMSG_WRAP_H_INCLUDED__
#define __QUICKMSG_WRAP_H_INCLUDED__

#include <quickmsg/ctypes.h>

#ifdef __cplusplus
extern "C" {
#endif


  void 
  qm_init(const char* name);

  void 
  qm_shutdown(const char* reason);

  bool
  qm_ok();

  double
  qm_get_msg_stamp(qm_message_t *self_p);

  const char* 
  qm_get_msg_src(qm_message_t *self_p);

  const char* 
  qm_get_msg_str(qm_message_t *self_p);

  bool
  qm_get_successful(qm_service_reply_t* self_p);

#ifdef __cplusplus
}
#endif

#endif
