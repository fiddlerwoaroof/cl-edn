;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :cl-edn 
  :description ""
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:alexandria
               #:uiop
               #:serapeum
               #:smug)
  :serial t
  :in-order-to ((test-op (test-op :cl-edn/test)))
  :components ((:file "package")
               (:file "edn")
               (:file "synthesize")))

(defsystem :cl-edn/fset
  :depends-on (#:cl-edn
               #:fset
               #:fwoar.lisputils)
  :components ((:file "fset-synthesize")))

(defsystem :cl-edn/fset-lossy
  :depends-on (#:cl-edn
               #:cl-edn/fset
               #:fset
               #:fwoar.lisputils)
  :components ((:file "fset-lossy-synthesize")))

(defsystem :cl-edn/test
  :depends-on (#:should-test)
  :perform (test-op (o s)
                    (uiop:symbol-call :st '#:test
                                      :package :edn-test))
  :components ((:file "package")
               (:file "test")))
