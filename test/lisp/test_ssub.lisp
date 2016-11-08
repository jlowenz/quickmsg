(load "../../src/lisp/quickmsg.lisp")

(ql:quickload "quickmsg")
(defpackage :qm_sub
  (:use :common-lisp :cffi :cl-json :iterate)
  (:export main))
(in-package :qm_sub)

(defun echo-msg (msg)
  (format t "LISP subscriber callback~%")
  (format t "Message stamp:~% ~S ~%    contents:~% ~S" 
	  (qm:get-msg-stamp msg)
	  (qm:get-msg-str msg)))

(defun main ()  
  ;; default network iface name is "". Should be set based on network configuration
  (qm:init "test_sub" "")
  (let ((sub (qm:subscriber-new "chatter" 10)))
    (loop while (qm:ok) do
       (let* ((v (qm:subscriber-get-messages sub))
	      (sz (qm:msgvec-size v)))
	 (if (> sz 0)
	     (loop for i from 0 to (- sz 1) do
		  (echo-msg (qm:msgvec-get v i))))
	 (qm:msgvec-destroy v)
	 (sleep 1)))
    (qm:subscriber-destroy sub)))

(main)
