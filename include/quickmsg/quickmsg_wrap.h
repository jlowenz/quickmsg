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

  int
  qm_ok();

  // TODO: fix the inconsistencies

  double
  qm_get_message_stamp(qm_message_t self_p);

  const char* 
  qm_get_message_src(qm_message_t self_p);

  const char* 
  qm_get_message_str(qm_message_t self_p);

  void 
  qm_message_destroy(qm_message_t self_p);

  int
  qm_get_successful(qm_service_reply_t self_p);
  
  void
  qm_free_string(char* str);

#ifdef __cplusplus
}
#endif

#endif
