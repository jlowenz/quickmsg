(require :cffi)
(require :cl-json)
(require :iterate)
(defpackage :qm
  (:use :common-lisp :cffi :cl-json :iterate)
  (:export publisher-new
           publisher-destroy
           publish
           subscriber-new
           subscriber-destroy
           client-new
           client-destroy
           call-srv
           service-new
           service-destroy
           service-spin
           get-msg-stamp
           get-msg-str
           ok
	   init
	   shutdown))
(in-package :qm)

(setf json:*json-identifier-name-to-lisp* 'json:simplified-camel-case-to-lisp)

(cffi:define-foreign-library libqm
    (t (:default "libquickmsg")))
(cffi:use-foreign-library libqm)

;; Publisher

(cffi:defcfun ("qm_publisher_new" publisher-new) :pointer
  (topic :string))

(cffi:defcfun ("qm_publisher_destroy" publisher-destroy) :void
  (self_p :pointer))

(cffi:defcfun ("qm_publish" publish) :void
  (self_p :pointer)
  (msg :string))

;; Subscriber
(cffi:defcfun ("qm_subscriber_new" subscriber-new) :pointer 
  (topic :string)
  (impl :pointer))

(cffi:defcfun ("qm_subscriber_destroy" subscriber-destroy) :void
  (self_p :pointer))

;; Client
(cffi:defcfun ("qm_client_new" client-new) :pointer
  (topic :string))

(cffi:defcfun ("qm_client_destroy" client-destroy) :void
  (self_p :pointer))

(cffi:defcfun ("qm_call_srv" call-srv) :string
  (self_p :pointer)
  (msg :string))

;; Server
(cffi:defcfun ("qm_service_new" service-new) :pointer
  (topic :string)
  (impl :pointer))

(cffi:defcfun ("qm_service_destroy" service-destroy) :void
  (self_p :pointer))

(cffi:defcfun ("qm_service_spin" service-spin) :void
  (self_p :pointer))

;; Message
(cffi:defcfun ("qm_get_msg_stamp" get-msg-stamp) :double
  (self_p :pointer))

(cffi:defcfun ("qm_get_msg_str" get-msg-str) :string
  (self_p :pointer))

;; Misc
(cffi:defcfun ("qm_ok" ok) :boolean )

(cffi:defcfun ("qm_init" init) :void 
  (node-name :string))

(cffi:defcfun ("qm_shutdown" shutdown) :void
  (reason :string))
