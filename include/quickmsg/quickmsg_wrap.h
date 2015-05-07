#ifndef __QUICKMSG_WRAP_H_INCLUDED__
#define __QUICKMSG_WRAP_H_INCLUDED__

#ifdef __cplusplus
extern "C" {
#endif

//  Opaque class structure
typedef struct _qmg_message_t qmg_message_t;

void 
qmg_init(const std::string& name);

void 
qmg_shutdown(const std::string& reason);

/* qmg_message_t *  */ // Dont actually need to create messages
/* qmg_message_new(); */

/* void  */
/* qmg_message_destroy(qmg_message_t *self_p); */

double
qmg_message_get_stamp(qmg_message_t *self_p);

const char* 
qmg_message_get_src(qmg_message_t *self_p);

const char* 
qmg_message_get_msg_str(qmg_message_t *self_p);

#ifdef __cplusplus
}
#endif

#endif
