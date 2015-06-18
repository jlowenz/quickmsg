;;;; quickmsg.asd
;;;; ASDF System definition

(asdf:defsystem :quickmsg
    :description "A Common Lisp binding for the Quickmsg simple messaging system"
    :author "Jason Owens <jason.l.owens.civ@mail.mil>"
    :author "Philip Osteen <philip.r.osteen.ctr@mail.mil>"
    :license "BSD"
    :serial t
    :depends-on (:cl-json :iterate :cffi)
    :components ((:file "quickmsg")))

