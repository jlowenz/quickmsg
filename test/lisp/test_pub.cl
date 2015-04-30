(require :cffi)
(require :cl-json)
(require :iterate)
(defpackage :qmg_pub
  (:use :common-lisp :cffi :cl-json :iterate))
(in-package :qmg_pub)

(pushnew #P"/home/phil/devel/quickmsg/build/" cffi:*foreign-library-directories*) ;; need to fix this hard-coded nonsense
(cffi:define-foreign-library libqm
    (t (:default "libquickmsg")))
(cffi:use-foreign-library libqm)

(cffi:defcfun "qmg_publisher_new" :pointer
  (topic :string))

(cffi:defcfun "qmg_publisher_destroy" :void
  (self_p :pointer))

(cffi:defcfun "qmg_publish" :void
  (self_p :pointer)
  (msg :string))

(defparameter *pub* (qmg-publisher-new "chatter"))
(defparameter *msg* (json:encode-json-to-string
                     '((foo . (1 2 3)) (bar . "asdf"))))

(iterate (for i from 1 to 10)
         (qmg-publish *pub* *msg*)
         (sleep 1))

(qmg-publisher-destroy *pub*)

