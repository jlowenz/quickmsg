#ifndef __CLIENT_WRAP_H_INCLUDED__
#define __CLIENT_WRAP_H_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _qm_client_t qm_client_t;

qm_client_t *
qm_client_new (const char* srv_name);

void
qm_client_destroy (qm_client_t *self_p);

const char*
qm_call_srv(qm_client_t *self_p, const char* req);

//  Self test of this class
void
qm_client_test (bool verbose);


#ifdef __cplusplus
}
#endif

#endif
