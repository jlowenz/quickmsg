;(load "../../src/lisp/quickmsg.lisp")

(defpackage :qm_service
  (:use :common-lisp :cffi :cl-json :iterate :quickmsg)
  (:export :main))
(in-package :qm_service)

(cffi:defcallback add-ints :string ((req :pointer))
  (let* ((req-json (json:decode-json-from-string (qm:get-msg-str req)))
	 (addends (cdr (assoc :ints-to-add req-json)))
	 (result (apply '+ addends))
	 (resp-alist (acons 'ints-to-add addends 
			    (acons 'result result nil)))
	 (resp-str (json:encode-json-to-string resp-alist)))
    (format t "Lisp service callback~%")
    (format t "Message stamp: ~f~%Contents: ~S~%" 
	    (qm:get-msg-stamp req)
	    (qm:get-msg-str req))    
    resp-str))

(defun main ()
  (qm:init "test-service") ; should only be called once
  (let ((svc (qm:service-new "add" (cffi:callback add-ints))))
    (qm:service-spin svc)
    (qm:service-destroy svc)))

(main)
