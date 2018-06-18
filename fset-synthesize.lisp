(in-package :edn)

(defmethod synthesize ((implementation (eql 'fset)) thing)
  (typecase thing
    (list (synthesize-compound implementation (car thing) (cdr thing)))
    (t thing)))


(defmethod synthesize-compound ((implementation (eql 'fset)) (discriminator (eql :map)) args)
  (fset:convert 'fset:map
                (mapcar (fw.lu:destructuring-lambda ((p k v))
                          (declare (ignore p))
                          (cons (synthesize implementation k)
                                (synthesize implementation v)))
                        args)))

(defmethod synthesize-compound ((implementation (eql 'fset)) (discriminator (eql :set)) args)
  (fset:convert 'fset:set
                (mapcar (lambda (a)
                          (synthesize implementation a))
                        args)))

(defmethod synthesize-compound ((implementation (eql 'fset)) (discriminator (eql :vector)) args)
  (fset:convert 'fset:seq
                (mapcar (lambda (a)
                          (synthesize implementation a))
                        args)))

(defmethod synthesize-compound ((implementation (eql 'fset)) (discriminator (eql :list)) args)
  (mapcar (lambda (a)
            (synthesize implementation a))
          args))

(defmethod synthesize-compound (implementation (discriminator (eql :keyword)) args)
  (destructuring-bind (ns name) args
    (alexandria:make-keyword (if ns
                                 (format nil "~a/~a" ns name)
                                 (format nil "~a" name)))))

(defmethod synthesize-compound (implementation (discriminator (eql :string)) args)
  (car args))

(defmethod synthesize-compound (implementation (discriminator (eql :symbol)) args)
  (destructuring-bind (ns name) args
    (make-symbol (if ns
                     (format nil "~a/~a" ns name)
                     (format nil "~a" name)))))

(defmethod synthesize-compound (implementation (discriminator (eql :tagged)) args)
  (destructuring-bind (sym obj) args
    (list :tagged (synthesize-compound implementation (car sym) (cdr sym))
          (synthesize implementation obj))))
