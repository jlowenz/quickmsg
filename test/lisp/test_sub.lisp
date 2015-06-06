(load "../../src/lisp/quickmsg.lisp")

;; (defpackage :qm_sub
;;   (:use :common-lisp :cffi :cl-json :iterate)
;;   (:export main))
;; (in-package :qm_sub)

(cffi:defcallback echo-msg :void ((msg :pointer))
		  (format t "LISP subscriber callback~%")
		  (format t "Message stamp:~% ~S ~%    contents:~% ~S" 
			  (qm:get-msg-stamp msg)
			  (qm:get-msg-str msg)))

(defun main ()  
  (qm:init "test_sub")
  (let ((sub (qm:async-subscriber-new "chatter" 
				      (cffi:callback echo-msg) 
				      (cffi:null-pointer))))	
    (qm:async-subscriber-spin sub)
    (qm:async-subscriber-destroy sub)))

(main)
