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
  (queue_sz :int))

(cffi:defcfun "qmg_subscriber_destroy" :void
  (self_p :pointer))

(defparameter *sub* (qmg-subscriber-new "chatter" 10))

(loop (sleep 1))

(qmg-subscriber-destroy *sub*)
