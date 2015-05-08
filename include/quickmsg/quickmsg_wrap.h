#ifndef __QUICKMSG_WRAP_H_INCLUDED__
#define __QUICKMSG_WRAP_H_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _qm_message_t qm_message_t;

void 
qm_init(const std::string& name);

void 
qm_shutdown(const std::string& reason);

bool
qm_ok();

double
qm_get_msg_stamp(qm_message_t *self_p);

const char* 
qm_get_msg_src(qm_message_t *self_p);

const char* 
qm_get_msg_str(qm_message_t *self_p);

#ifdef __cplusplus
}
#endif

#endif
