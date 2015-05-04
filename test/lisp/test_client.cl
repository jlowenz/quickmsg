(require :cffi)
(require :cl-json)
(require :iterate)
(defpackage :qmg_client
  (:use :common-lisp :cffi :cl-json :iterate))
(in-package :qmg_client)

(pushnew #P"/home/phil/devel/quickmsg/build/" cffi:*foreign-library-directories*) ;; need to fix this hard-coded nonsense
(cffi:define-foreign-library libqm
    (t (:default "libquickmsg")))
(cffi:use-foreign-library libqm)

(cffi:defcfun "qmg_client_new" :pointer
  (topic :string))

(cffi:defcfun "qmg_client_destroy" :void
  (self_p :pointer))

(cffi:defcfun "qmg_call_srv" :string
  (self_p :pointer)
  (msg :string))


(defparameter *client* (qmg-client-new "add"))
(defparameter *req* (json:encode-json-to-string
                      '((ints_to_add . (1 2 3)) (bar . "asdf"))))
(defvar *resp*)
(iterate (for i from 1 to 10)
         (setf *resp* (qmg-call-srv *client* *req*))
         (print "lisp test client received ")
         (print *resp*)
         (sleep 1))

(qmg-client-destroy *client*)
