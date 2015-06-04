(load "../../src/lisp/quickmsg.lisp")

(defpackage :qm_service
  (:use :common-lisp :cffi :cl-json :iterate)
  (:export :main))
(in-package :qm_service)

(qm:init "test-service") ; should only be called once

(cffi:defcallback add-ints :string ((req :pointer))
  (let* ((req-json (json:decode-json-from-string (qm:get-msg-str req)))
	(ints-to-add (cdr (assoc 'ints_to_add req-json)))
	(result (apply '+ ints-to-add))
	(resp-alist (acons 'ints-to-add ints-to-add 
			   (acons 'result result nil)))
	(resp-str (json:encode-json-to-string resp-alist)))
    (format t "Lisp service callback~%")
    (format t "Message stamp: ~f~%Contents: ~S~%" 
	    (qm:get-msg-stamp req)
	    (qm:get-msg-str req))    
    resp-str))

(defun main ()
  (defparameter *service* (qm:service-new "add" (cffi:callback add-ints)))
  (qm:service-spin *service*)
  (qm:service-destroy *service*))
