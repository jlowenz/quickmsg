(load "../../src/quickmsg.cl")

(defpackage :qm_service
  (:use :common-lisp :cffi :cl-json :iterate))
(in-package :qm_service)

(defvar *req-json*)
(defvar *ints-to-add*)
(defvar *result*)
(defvar *resp-alist*)
(defvar *resp-str*)

(cffi:defcallback add-ints :string ((req :pointer))
  (print "LISP service callback")
  (setf *req-json* (json:decode-json-from-string (qm:get-msg-str req)))
  (format t "Message stamp:~% ~S ~%    contents:~% ~S" 
          (qm:get-msg-stamp req)
          (qm:get-msg-str req))
  (setf *ints-to-add* (cdr (assoc ':ints_to_add *req-json*)))
  (setf *result* (apply '+ *ints-to-add*))
  (setf *resp-alist* (acons 'ints_to_add *ints-to-add* 
                                    (acons 'result *result* nil)))
  (setf *resp-str* (json:encode-json-to-string *resp-alist*)))


(defparameter *service* (qm:service-new "add" (cffi:callback add-ints)))
(qm:service-spin *service*)
(qm:service-destroy *service*)
