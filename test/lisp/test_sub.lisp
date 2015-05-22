(load "../../src/lisp/quickmsg.lisp")

(defpackage :qm_sub
  (:use :common-lisp :cffi :cl-json :iterate))
(in-package :qm_sub)

(cffi:defcallback echo-msg :string ((msg :pointer))
  (print "LISP subscriber callback")
  (format t "Message stamp:~% ~S ~%    contents:~% ~S" 
          (qm:get-msg-stamp msg)
          (qm:get-msg-str msg)))

(defparameter *sub* (qm:subscriber-new "chatter" (cffi:callback echo-msg)))

(loop while (qm:ok) do (sleep 1))

(qm:subscriber-destroy *sub*)
