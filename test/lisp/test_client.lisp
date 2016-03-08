;(load "../../src/lisp/quickmsg.lisp")

(ql:quickload "quickmsg")
(defpackage :qm_client
  (:use :common-lisp :cl-json :iterate :quickmsg)
  (:export :main))
(in-package :qm_client)

(defun main ()
  ;; default network iface name is "". Should be set based on network configuration
  (qm:init "test-client" "")
  (let ((client (qm:client-new "add"))
	(req (json:encode-json-to-string
	      '((ints-to-add . (1 2 3)) (bar . "asdf")))))
    (iterate (while (qm:ok))
	     (for i from 1 to 10)
	     (let ((resp (handler-case (qm:call-srv client req)
			   (qm:service-call-timeout () nil))))
	       (format t "lisp test client received~%")
	       (format t "response: ~s~%" resp)
	       (sleep 1)))
    (qm:client-destroy client)))

(main)
