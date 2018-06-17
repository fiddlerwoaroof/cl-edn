(defpackage :edn-test
  (:use :cl :st)
  (:export ))
(in-package :edn-test)

(deftest edn-parser ()
  (should be equal
          '((:map (:pair 1 1)))
          (smug:parse (edn::.elements) "{ 1 1 }"))
  (should be equal
          '((:vector 1 1))
          (smug:parse (edn::.elements) "[ 1 1 ]"))
  (should be equal
          '((:set 1 1))
          (smug:parse (edn::.elements) "#{ 1   1 }"))
  (should be equal
          '((:tagged (:symbol nil "foobar") (:vector 1 1)))
          (smug:parse (edn::.elements) "#foobar [ 1 1 ]"))
  (should be equal
          '(:list
            (:set
             (:vector
              (:map (:pair edn-primitives:nil edn-primitives:true)
                    (:pair edn-primitives:false edn-primitives:nil)))))
          (car (smug:parse (smug:.prog1 (edn::.elements) (smug:.not (smug:.item)))
                           "(#{[{nil true,false nil}]})")))
  )
