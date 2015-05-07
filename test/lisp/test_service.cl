(require :cffi)
(require :cl-json)
(require :iterate)
(defpackage :qmg_service
  (:use :common-lisp :cffi :cl-json :iterate))
(in-package :qmg_service)

(pushnew #P"/home/phil/devel/quickmsg/build/" cffi:*foreign-library-directories*) ;; need to fix this hard-coded nonsense
(cffi:define-foreign-library libqm
  (t (:default "libquickmsg")))
(cffi:use-foreign-library libqm)

(cffi:defcfun "qmg_service_new" :pointer
  (topic :string)
  (impl :pointer))

(cffi:defcfun "qmg_service_destroy" :void
  (self_p :pointer))

(cffi:defcfun "qmg_service_spin" :void
  (self_p :pointer))

(cffi:defcfun "qmg_message_get_stamp" :double
  (self_p :pointer))

(cffi:defcfun "qmg_message_get_msg_str" :string
  (self_p :pointer))

(setf json:*json-identifier-name-to-lisp* 'json:simplified-camel-case-to-lisp)

(defvar *req-json*)
(defvar *ints-to-add*)
(defvar *result*)
(defvar *resp-alist*)
(defvar *resp-str*)

(cffi:defcallback add-ints :string ((req :pointer))
  (print "LISP service callback")
  (setf *req-json* (json:decode-json-from-string (qmg-message-get-msg-str req)))
  (format t "Message stamp:~% ~S ~%    contents:~% ~S" 
          (qmg-message-get-stamp req)
          (qmg-message-get-msg-str req))
  (setf *ints-to-add* (cdr (assoc ':ints_to_add *req-json*)))
  (setf *result* (apply '+ *ints-to-add*))
  (setf *resp-alist* (acons 'ints_to_add *ints-to-add* 
                                    (acons 'result *result* nil)))
  (setf *resp-str* (json:encode-json-to-string *resp-alist*)))


(defparameter *service* (qmg-service-new "add" (cffi:callback add-ints)))
(qmg-service-spin *service*)
(qmg-service-destroy *service*)
