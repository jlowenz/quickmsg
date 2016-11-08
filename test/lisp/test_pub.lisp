(load "../../src/lisp/quickmsg.lisp")

(ql:quickload "quickmsg")
(defpackage :qm_pub
  (:use :common-lisp :cffi :cl-json :iterate))
(in-package :qm_pub)

(defun main ()
  ;; default network iface name is "". Should be set based on network configuration
  (qm:init "test_pub" "")
  (let ((pub (qm:publisher-new "chatter" :wait)))
    (iterate:iterate (iterate:while (qm:ok))
	     (for i from 1 to 10)
	     (qm:publish pub (format nil "Hello World ~d~%" i))
	     (sleep 0.5))
    (qm:publisher-destroy pub)))

(main)

