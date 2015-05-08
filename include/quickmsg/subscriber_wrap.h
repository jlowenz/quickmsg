#ifndef __SUBSCRIBER_WRAP_H_INCLUDED__
#define __SUBSCRIBER_WRAP_H_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _qm_subscriber_t qm_subscriber_t;

qm_subscriber_t *
qm_subscriber_new (const char* topic, int queue_sz);

void
qm_subscriber_destroy (qm_subscriber_t *self_p);

//  Self test of this class
void
qm_subscriber_test (bool verbose);


#ifdef __cplusplus
}
#endif

#endif
