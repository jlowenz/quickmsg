(load "../../src/lisp/quickmsg.lisp")

(use-package :iterate)

;; (defpackage :qm_pub
;;   (:use :common-lisp :cffi :cl-json :iterate))
;; (in-package :qm_pub)

(defun main ()
  (qm:init "test_pub")
  (let ((pub (qm:publisher-new "chatter")))
    (iterate:iterate (iterate:while (qm:ok))
	     (for i from 1 to 10)
	     (qm:publish pub (format nil "Hello World ~d~%" i))
	     (sleep 1))
    (qm:publisher-destroy pub)))

(main)

