#ifndef __CLIENT_WRAP_H_INCLUDED__
#define __CLIENT_WRAP_H_INCLUDED__

#include <quickmsg/ctypes.h>

#ifdef __cplusplus
extern "C" {
#endif

  qm_client_t
  qm_client_new (const char* srv_name);

  void
  qm_client_destroy (qm_client_t self_p);

  /**
   * Call the service with the given request. Returns ownership to a
   * string that represents the response from the service in the resp
   * OUT argument. Returns 0 on success, non-zero on service timeout.
   */
  int 
  qm_call_srv(qm_client_t self_p, const char* req, char** out_resp);

#ifdef __cplusplus
}
#endif

#endif
