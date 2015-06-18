(require :cffi)
(require :cl-json)
(require :iterate)
(defpackage :quickmsg
  (:nicknames :qm)
  (:use :common-lisp :cffi :cl-json :iterate)
  (:export publisher-new
           publisher-destroy
           publish
           subscriber-new
	   subscriber-get-messages
           subscriber-destroy
	   async-subscriber-new
	   async-subscriber-spin
	   async-subscriber-destroy
           client-new
           client-destroy
           call-srv
	   service-call-timeout
           service-new
           service-destroy
           service-spin
           get-msg-stamp
           get-msg-str
           ok
	   init
	   shutdown))
(in-package :quickmsg)

(setf json:*json-identifier-name-to-lisp* 'json:simplified-camel-case-to-lisp)

(cffi:define-foreign-library libqm
  (:unix (:default "libcquickmsg"))
  (:windows "quickmsg-c.dll"))
(cffi:load-foreign-library 'libqm)
;(cffi:use-foreign-library 'libqm)

(cffi:defcfun ("qm_alloc_string" alloc-string) :pointer (size :int))
(cffi:defcfun ("qm_free_string" free-string) :void (str :pointer))

;; Publisher

(cffi:defcfun ("qm_publisher_new" publisher-new) :pointer
  (topic :string))

(cffi:defcfun ("qm_publisher_destroy" publisher-destroy) :void
  (self_p :pointer))

(cffi:defcfun ("qm_publish" publish) :void
  (self_p :pointer)
  (msg :string))

;; Subscriber
(cffi:defcfun ("qm_subscriber_new" subscriber-new) :pointer 
  (topic :string)
  (queue-size :int))

(cffi:defcfun ("qm_subscriber_get_messages" subscriber-get-messages) :pointer
  (self_p :pointer))

(cffi:defcfun ("qm_subscriber_destroy" subscriber-destroy) :void
  (self_p :pointer))

;; (cffi:defcfun ("qm_async_subscriber_new" async-subscriber-new) :pointer
;;   (topic :string)
;;   (handler :pointer)
;;   (args :pointer))

					;(declaim (special *async-callback-handler*))
					;(defvar *async-callback-handler* (make-hash-table))

					;(declare (special *async-callback-handler*))
;(defvar *async-callback-handler* (lambda (m) (format t "what????~%")))
					;(defun get-handler () (symbol-value '*async-callback-handler*))
(defvar *async-callback-handler* nil)
(declaim (special *async-callback-handler*))
(cffi:defcallback handler-cb :void ((msg :pointer))
		  (funcall (symbol-value '*async-callback-handler*) msg))

(defun async-subscriber-new (topic handler-fn)
  (setf *async-callback-handler* handler-fn)
					;  (let ((*async-callback-handler* handler-fn))
					;  (declare (special *async-callback-handler*))    
  (format t "handler: ~a~%" *async-callback-handler*)
  (cffi:with-foreign-string (ftopic topic)
    (cffi:foreign-funcall "qm_async_subscriber_new"
			  :pointer ftopic
			  :pointer (cffi:callback handler-cb)
			  :pointer (cffi:null-pointer)
			  :pointer)))

(cffi:defcfun ("qm_async_subscriber_spin" async-subscriber-spin) :void
  (self_p :pointer))

(cffi:defcfun ("qm_async_subscriber_destroy" async-subscriber-destroy) :void
  (self_p :pointer))

;; Client
(cffi:defcfun ("qm_client_new" client-new) :pointer
  (topic :string))

(cffi:defcfun ("qm_client_destroy" client-destroy) :void
  (self_p :pointer))

; define an error condition when the service call times out
(define-condition service-call-timeout (error)
  ((request :initarg :request :reader request)))

(defun call-srv (self-p request)
  "Call the service with the given request string"  
  (let ((resp-ptr (cffi:foreign-alloc :pointer)))
    (cffi:with-foreign-string (req-str request)      
      (let* ((ret (cffi:foreign-funcall "qm_call_srv" 
				       :pointer self-p
				       :pointer req-str
				       :pointer resp-ptr
				       :int))
	     (resp-str (if (= ret 0) (foreign-string-to-lisp (mem-ref resp-ptr :pointer)) "")))
	(if (= ret 0) 
	    (progn 	      
	      (free-string (mem-ref resp-ptr :pointer)) ; free the string and the pointer we alloc'd
	      (cffi:foreign-free resp-ptr)
	      resp-str) ; return the value
	    (progn
	      (cffi:foreign-free resp-ptr)
	      (error 'service-call-timeout :request request)))))))
;; Server


(defvar *service-callback-handler* nil)
(declaim (special *service-callback-handler*))
(cffi:defcallback service-cb
    :pointer ((msg :pointer))
    (let* ((resp (funcall (symbol-value '*service-callback-handler*) msg))
	   (resp-len (+ 1 (length resp)))
	   (resp-ptr (alloc-string resp-len)))
      (format t "Copying to native string~%")
      (cffi:lisp-string-to-foreign resp resp-ptr resp-len)
      resp-ptr)) ;; need to return a foreign-allocated pointer, since we are returning ownership

(defun service-new (topic handler-fn)
  (setf *service-callback-handler* handler-fn)
  (cffi:foreign-funcall "qm_service_new"
			:string topic
			:pointer (cffi:callback service-cb)
			:pointer (cffi:null-pointer)
			:pointer))
;; (cffi:defcfun ("qm_service_new" service-new) :pointer
;;   (topic :string)
;;   (impl :pointer))

(cffi:defcfun ("qm_service_destroy" service-destroy) :void
  (self_p :pointer))

(cffi:defcfun ("qm_service_spin" service-spin) :void
  (self_p :pointer))

;; Message
(cffi:defcfun ("qm_get_message_stamp" get-msg-stamp) :double
  (self_p :pointer))

;; (cffi:defcfun ("qm_get_message_str" get-msg-str) :string
;;   (self_p :pointer))
(defun get-msg-str (self-p)
  (let ((c-str (cffi:foreign-funcall "qm_get_message_str"
				     :pointer self-p
				     :pointer)))
    (format t "get-msg-str: ~a~%" c-str)
    (cffi:foreign-string-to-lisp c-str)))

;; Misc
(cffi:defcfun ("qm_ok" ok) :boolean )

(cffi:defcfun ("qm_init" init) :void 
  (node-name :string))

(cffi:defcfun ("qm_shutdown" shutdown) :void
  (reason :string))
