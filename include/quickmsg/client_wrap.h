#ifndef __CLIENT_WRAP_H_INCLUDED__
#define __CLIENT_WRAP_H_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _qmg_client_t qmg_client_t;

qmg_client_t *
qmg_client_new (const char* srv_name);

void
qmg_client_destroy (qmg_client_t *self_p);

const char*
qmg_call_srv(qmg_client_t *self_p, const char* req);

//  Self test of this class
void
qmg_client_test (bool verbose);


#ifdef __cplusplus
}
#endif

#endif
