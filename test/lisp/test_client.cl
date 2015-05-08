(load "../../src/quickmsg.cl")

(defpackage :qm_client
  (:use :common-lisp :cffi :cl-json :iterate))
(in-package :qm_client)

(defvar *client* (qm:client-new "add"))
(defvar *req* (json:encode-json-to-string
               '((ints_to_add . (1 2 3)) (bar . "asdf"))))
(defvar *resp*)

(iterate (while (qm:ok))
         (for i from 1 to 10)
         (setf *resp* (qm:call-srv *client* *req*))
         (print "lisp test client received ")
         (print *resp*)
         (sleep 1))

(qm:client-destroy *client*)
