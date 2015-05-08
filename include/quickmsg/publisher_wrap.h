#ifndef __PUBLISHER_WRAP_H_INCLUDED__
#define __PUBLISHER_WRAP_H_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _qm_publisher_t qm_publisher_t;

qm_publisher_t *
qm_publisher_new (const char* topic);

void
qm_publisher_destroy (qm_publisher_t *self_p);

void 
qm_publish(qm_publisher_t *self_p, const char* msg);

//  Self test of this class
void
qm_publisher_test (bool verbose);


#ifdef __cplusplus
}
#endif

#endif
