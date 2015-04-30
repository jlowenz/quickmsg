#ifndef __SUBSCRIBER_WRAP_H_INCLUDED__
#define __SUBSCRIBER_WRAP_H_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _qmg_subscriber_t qmg_subscriber_t;

qmg_subscriber_t *
qmg_subscriber_new (const char* topic, int queue_sz);

void
qmg_subscriber_destroy (qmg_subscriber_t *self_p);

//  Self test of this class
void
qmg_subscriber_test (bool verbose);


#ifdef __cplusplus
}
#endif

#endif
