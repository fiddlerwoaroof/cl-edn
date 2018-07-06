(defpackage :edn-test
  (:use :cl :st)
  (:export ))
(in-package :edn-test)

(defun float-equal (a b)
  (> 0.00001
     (abs (- a b))))

(deftest floating ()
  (should be float-equal
          0.1
          (edn:read-edn "0.1"))
  (should be float-equal
          0.1
          (edn:read-edn "+0.1"))
  (should be float-equal
          -0.1
          (edn:read-edn "-0.1"))
  (should be float-equal
          1
          (edn:read-edn "0.1e1"))
  (should be float-equal
          1
          (edn:read-edn "0.1e+1"))
  (should be float-equal
          0.01
          (edn:read-edn "0.1e-1"))
  (should be float-equal
          -0.01
          (edn:read-edn "-0.1e-1"))
  (should be float-equal
          -0.01
          (edn:read-edn "-0.1e-1M"))
  (should be float-equal
          -0.0
          (edn:read-edn "-0.e-1M")))

(deftest edn-parser ()
  (should be equal
          '(:list)
          (edn:read-edn (format nil "()~%")))
  (should be equal
          '(:map (:pair 1 1))
          (edn:read-edn "{ 1 1 }"))
  (should be equal
          '(:vector 1 1)
          (edn:read-edn "[ 1 1 ]"))
  (should be equal
          '(:set 1 1)
          (edn:read-edn "#{ 1   1 }"))
  (should be equal
          '(:tagged (:symbol nil "foobar") (:vector 1 1))
          (edn:read-edn "#foobar [ 1 1 ]"))
  (should be equal
          '(:list
            (:set
             (:vector
              (:map (:pair edn-primitives:nil edn-primitives:true)
                    (:pair edn-primitives:false edn-primitives:nil)))))
          (edn:read-edn "(#{[{nil true,false nil}]})")))
