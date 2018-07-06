(in-package :edn)
(defclass fset ()
  ()
  (:documentation "An EDN synthesizer that produces fset datastructures"))


(defmethod synthesize ((implementation fset) thing)
  (typecase thing
    (list (synthesize-compound implementation (car thing) (cdr thing)))
    (t thing)))


(defmethod synthesize-compound ((implementation fset) (discriminator (eql :map)) args)
  (fset:convert 'fset:map
                (mapcar (fw.lu:destructuring-lambda ((p k v))
                          (declare (ignore p))
                          (cons (synthesize implementation k)
                                (synthesize implementation v)))
                        args)))

(defmethod synthesize-compound ((implementation fset) (discriminator (eql :set)) args)
  (fset:convert 'fset:set
                (mapcar (lambda (a)
                          (synthesize implementation a))
                        args)))

(defmethod synthesize-compound ((implementation fset) (discriminator (eql :vector)) args)
  (fset:convert 'fset:seq
                (mapcar (lambda (a)
                          (synthesize implementation a))
                        args)))

(defmethod synthesize-compound ((implementation fset) (discriminator (eql :list)) args)
  (mapcar (lambda (a)
            (synthesize implementation a))
          args))

