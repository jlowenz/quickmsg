(require :cffi)
(require :cl-json)
(require :iterate)
(defpackage :qmg_sub
  (:use :common-lisp :cffi :cl-json :iterate))
(in-package :qmg_sub)

(pushnew #P"/home/phil/devel/quickmsg/build/" cffi:*foreign-library-directories*)
(cffi:define-foreign-library libqm
    (t (:default "libquickmsg")))
(cffi:use-foreign-library libqm)

(cffi:defcfun "qmg_subscriber_new" :pointer 
  (topic :string)
  (impl :pointer))

(cffi:defcfun "qmg_subscriber_destroy" :void
  (self_p :pointer))

(cffi:defcfun "qmg_message_get_stamp" :double
  (self_p :pointer))

(cffi:defcfun "qmg_message_get_msg_str" :string
  (self_p :pointer))

(cffi:defcallback echo-msg :string ((msg :pointer))
  (print "LISP subscriber callback")
  (format t "Message stamp:~% ~S ~%    contents:~% ~S" 
          (qmg-message-get-stamp msg)
          (qmg-message-get-msg-str msg)))

(defparameter *sub* (qmg-subscriber-new "chatter" (cffi:callback echo-msg)))

(loop (sleep 1))

(qmg-subscriber-destroy *sub*)
