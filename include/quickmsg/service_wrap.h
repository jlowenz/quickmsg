#ifndef __SERVICE_WRAP_H_INCLUDED__
#define __SERVICE_WRAP_H_INCLUDED__

#include <quickmsg/types.hpp>

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _qmg_service_t qmg_service_t;

qmg_service_t *
qmg_service_new (const char* srv_name, 
                 const char* (*impl)(const quickmsg::Message* req));

void
qmg_service_destroy (qmg_service_t *self_p);

void
qmg_service_spin(qmg_service_t *self_p);

//  Self test of this class
void
qmg_service_test (bool verbose);


#ifdef __cplusplus
}
#endif

#endif
