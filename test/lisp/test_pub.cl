(load "../../src/quickmsg.cl")

(defpackage :qm_pub
  (:use :common-lisp :cffi :cl-json :iterate))
(in-package :qm_pub)

(defvar *pub* (qm:publisher-new "chatter"))
(defvar *msg* (json:encode-json-to-string
               '((foo . (1 2 3)) (bar . "asdf"))))

(iterate (while (qm:ok))
         (for i from 1 to 10)
         (qm:publish *pub* *msg*)
         (sleep 1))

(qm:publisher-destroy *pub*)

