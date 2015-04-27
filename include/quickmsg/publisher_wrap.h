#ifndef __PUBLISHER_WRAP_H_INCLUDED__
#define __PUBLISHER_WRAP_H_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _qmg_publisher_t qmg_publisher_t;

qmg_publisher_t *
qmg_publisher_new (const std::string& topic);

void
qmg_publisher_destroy (qmg_publisher_t *self_p);

void 
qmg_publish(qmg_publisher_t *self_p, const std::string& msg);

//  Self test of this class
void
qmg_publisher_test (bool verbose);


#ifdef __cplusplus
}
#endif

#endif
