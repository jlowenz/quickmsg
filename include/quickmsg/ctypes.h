#ifndef __QM_CTYPES_H_
#define __QM_CTYPES_H_

#include <stdlib.h>


#ifdef __cplusplus
extern "C" {
#endif

  //  Opaque class structures
  
  // Peer
  typedef struct _qm_peer* qm_peer_t;

  //  Message 
  typedef const struct _qm_message* qm_message_t;
  
  //  ServiceReply is a subclass of Message, so message functions work on it!
  typedef struct _qm_service_reply* qm_service_reply_t;
  
  //  Client handle
  typedef struct _qm_client_t* qm_client_t;

  //  Service handle
  typedef struct _qm_service_t* qm_service_t;

  //  Publisher handle
  typedef struct _qm_publisher_t* qm_publisher_t;

  //  Subscriber handle
  typedef struct _qm_subscriber_t* qm_subscriber_t;

  //  Async Subscriber handle
  typedef struct _qm_async_subscriber_t* qm_async_subscriber_t;

  // message handlers
  typedef void (*qm_message_handler_t)(const qm_message_t, void*);
  typedef char* (*qm_service_handler_t)(const qm_message_t, void*);

  // List/Vector access (defines a SIZE and GET accessor for each type)
#define PTR(T) T*
#define VEC_OF(CPP_TYPE,C_TYPE) 					\
  typedef struct _qm_vec_##C_TYPE * qm_vec_##C_TYPE;			\
  inline size_t qm_vec_##C_TYPE##_size(qm_vec_##C_TYPE o);		\
  inline C_TYPE* qm_vec_##C_TYPE##_get(qm_vec_##C_TYPE o, size_t elem);	\
  inline void qm_vec_##C_TYPE##_destroy(qm_vec_##C_TYPE o);		\
  
  VEC_OF(quickmsg::Peer,qm_peer_t) // qm_vec_qm_peer_t
  VEC_OF(quickmsg::Message,qm_message_t) // qm_vec_qm_message_t


#ifdef __cplusplus
}
#endif


#endif
