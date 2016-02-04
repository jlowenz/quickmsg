#ifndef __QUICKMSG_WRAP_H_INCLUDED__
#define __QUICKMSG_WRAP_H_INCLUDED__

#include <quickmsg/ctypes.h>

#ifdef __cplusplus
extern "C" {
#endif


  QM_EXPORT void
  qm_init(const char* name, const char* iface);

  QM_EXPORT void
  qm_shutdown(const char* reason);

  QM_EXPORT int
  qm_ok();

  // TODO: fix the inconsistencies

  QM_EXPORT double
  qm_get_message_stamp(qm_message_t self_p);

  QM_EXPORT const char*
  qm_get_message_src(qm_message_t self_p);

  QM_EXPORT const char*
  qm_get_message_str(qm_message_t self_p);

  QM_EXPORT void 
  qm_message_destroy(qm_message_t self_p);

  QM_EXPORT int 
  qm_get_successful(qm_service_reply_t self_p);


  QM_EXPORT char*
  qm_alloc_string(int length);
  
  
  QM_EXPORT void 
  qm_free_string(char* str);

#ifdef __cplusplus
}
#endif

#endif
