(defpackage :edn
  (:use :cl :smug)
  (:shadow :parse)
  (:export :read-edn
           :synthesize
           :fset
           :fset-lossy
           :convert-primitive))

(defpackage :edn.generate
  (:use :cl)
  (:export :generate-edn))

(defpackage :edn-primitives
  (:use)
  (:export :nil :true :false))

(defconstant edn-primitives:nil 'edn-primitives:nil)
(defconstant edn-primitives:true 'edn-primitives:true)
(defconstant edn-primitives:false 'edn-primitives:false)
