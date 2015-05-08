#ifndef __SERVICE_WRAP_H_INCLUDED__
#define __SERVICE_WRAP_H_INCLUDED__

#include <quickmsg/types.hpp>

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _qm_service_t qm_service_t;

qm_service_t *
qm_service_new (const char* srv_name, 
                 const char* (*impl)(const quickmsg::Message* req));

void
qm_service_destroy (qm_service_t *self_p);

void
qm_service_spin(qm_service_t *self_p);

//  Self test of this class
void
qm_service_test (bool verbose);


#ifdef __cplusplus
}
#endif

#endif
