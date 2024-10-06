(defpackage :edn-test
  (:use :cl :st)
  (:export ))
(in-package :edn-test)

(defun float-equal (a b)
  (and (typep a (type-of b))
       (typep b (type-of a))
       (> 0.00001
          (abs (- a b)))))

(deftest floating ()
  (should be float-equal
          0.1d0
          (edn:read-edn "0.1")
          )
  (should be float-equal
          0.1d0
          (edn:read-edn "+0.1"))
  (should be float-equal
          -0.1d0
          (edn:read-edn "-0.1"))
  (should be float-equal
          1d0
          (edn:read-edn "0.1e1"))
  (should be float-equal
          1d0
          (edn:read-edn "0.1e+1"))
  (should be float-equal
          0.01d0
          (edn:read-edn "0.1e-1"))
  (should be float-equal
          -0.01d0
          (edn:read-edn "-0.1e-1"))
  (should be float-equal
          0d0
          (edn:read-edn "0M"))
  (should be float-equal
          -0.01d0
          (edn:read-edn "-0.1e-1M"))
  (should be float-equal
          -0.0d0
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

(deftest edn-parser-1 ()
  (should be equal
          '(:list)
          (fw.lu:closing
              (edn::parse-edn-new (make-string-input-stream
                                   (format nil "()~%")))))
  (should be equal
          '(:map (:pair 1 1))
          (fw.lu:closing
              (edn::parse-edn-new (make-string-input-stream
                                   "{ 1 1 }"))))
  (should be equal
          '(:vector 1 1)
          (fw.lu:closing
              (edn::parse-edn-new (make-string-input-stream
                                   "[ 1 1 ]"))))
  (should be equal
          '(:set 1 1)
          (fw.lu:closing
              (edn::parse-edn-new (make-string-input-stream
                                   "#{ 1   1 }"))))
  (should be equal
          '(:tagged (:symbol nil "foobar") (:vector 1 1))
          (fw.lu:closing
              (edn::parse-edn-new (make-string-input-stream
                                   "#foobar [ 1 1 ]"))))
  (should be equal
          '(:list
            (:set
             (:vector
              (:map (:pair edn-primitives:nil edn-primitives:true)
               (:pair edn-primitives:false edn-primitives:nil)))))
          (fw.lu:closing
              (edn::parse-edn-new (make-string-input-stream
                                   "(#{[{nil true,false nil}]})")))))

(deftest maps ()
  (should be equal
          '(:map (:pair 1 2))
          (edn:read-edn "{1 2 }"))
  (should be equal
          '(:map (:pair 1 2))
          (edn:read-edn "{ 1 2}"))
  (should be equal
          '(:map (:pair 1 2))
          (edn:read-edn "{1      2}"))
  (should be equal
          '(:map (:pair 1 2))
          (edn:read-edn "{ 1 2 }"))
  (should be equal
          '(:map (:pair 1 2))
          (edn:read-edn "{     1      2     }")))

(deftest translate-escape ()
  (flet ((translates-to (in out)
           (should be eql
                   out
                   (edn::translate-escape in))))
    (translates-to #\" #\")
    (translates-to #\\ #\\)
    (translates-to #\b (code-char 8))
    (translates-to #\f (code-char 12))
    (translates-to #\n (code-char 10))
    (translates-to #\r (code-char 13))
    (translates-to #\t (code-char 9))))

(deftest .string-ending ()
  (should be equal
          "foobar"
          ""
          (smug:parse (edn::.string-ending) "foobar\""))
  (should be equal
          "foobar"
          "asdf"
          (smug:parse (edn::.string-ending) "foobar\"asdf"))
  (should be equal
          "foobar\"qwer"
          "asdf"
          (smug:parse (edn::.string-ending) "foobar\\\"qwer\"asdf"))
  (should be equal
          (format nil "foobar~%qwer")
          "asdf"
          (smug:parse (edn::.string-ending) "foobar\\nqwer\"asdf")))

(deftest smoke ()
  (should be =
          0
          (loop repeat 0
             for res = (edn:read-edn (edn.generate:generate-edn))
             unless res sum 1)))
