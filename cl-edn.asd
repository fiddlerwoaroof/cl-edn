;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Package: ASDF-USER -*-
(in-package :asdf-user)

(defsystem :cl-edn 
  :description "A Common Lisp reader for EDN files"
  :author "Ed L <edward@elangley.org>"
  :license "MIT"
  :depends-on (#:alexandria
               #:uiop
               #:serapeum
               #:smug 
               :local-time
               :uuid
               :fwoar-lisputils)
  :in-order-to ((test-op (test-op :cl-edn/test)))
  :components ((:file "package")
               (:file "edn" :depends-on ("package" "synthesize"))
               (:file "synthesize"
                      :depends-on ("package"))))

(defsystem :cl-edn/fset
  :depends-on (#:cl-edn
               #:fset
               #:fwoar-lisputils)
  :components ((:file "fset-synthesize")))

(defsystem :cl-edn/fset-lossy
  :depends-on (#:cl-edn
               #:cl-edn/fset
               #:fset
               #:fwoar-lisputils)
  :components ((:file "fset-lossy-synthesize")))

(defsystem :cl-edn/test
  :depends-on (#:should-test)
  :perform (test-op (o s)
                    (uiop:symbol-call :st '#:test
                                      :package :edn-test))
  :components ((:file "package")
               (:file "generate-edn")
               (:file "test")))
