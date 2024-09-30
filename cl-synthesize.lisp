(in-package :edn)
(defclass edn-cl ()
  ()
  (:documentation "An EDN synthesizer that produces fset datastructures"))

(defvar *symbol-package*
  (make-package :edn.symbols :use ()))

(defmethod synthesize-compound ((implementation edn-cl) (discriminator (eql :map)) args)
  (fset:convert 'hash-table
                (fset:convert 'fset:map
                              (mapcar (fw.lu:destructuring-lambda ((p k v))
                                        (declare (ignore p))
                                        (cons (synthesize implementation k)
                                              (synthesize implementation v)))
                                      args))))

(defmethod synthesize-compound ((implementation edn-cl) (discriminator (eql :set)) args)
  (fset:convert 'fset:set
                (mapcar (lambda (a)
                          (synthesize implementation a))
                        args)))

(defmethod synthesize-compound ((implementation edn-cl) (discriminator (eql :symbol)) args)
  (destructuring-bind (ns name) args
    (intern (if ns
                (format nil "~a/~a" ns name)
                (format nil "~a" name))
            *symbol-package*)))

(defmethod synthesize-compound ((implementation edn-cl) (discriminator (eql :vector)) args)
  (map 'vector
       (lambda (a)
         (synthesize implementation a))
       args))

(defmethod synthesize-compound ((implementation edn-cl) (discriminator (eql :list)) args)
  (mapcar (lambda (a)
            (synthesize implementation a))
          args))


(fw.lu:defclass+ dbid ()
  ((%type :initarg :type :reader dbid-type)
   (%value :initarg :value :reader dbid-vlaue)))
(defvar *dbid-intern-table*
  (make-hash-table :test 'equal))

(defmethod synthesize-tag ((implementation edn-cl) (tag (eql :db/id)) args)
  (fw.lu:vector-destructuring-bind (type value) args
    (alexandria:ensure-gethash (list type value)
                               *dbid-intern-table*
                               (dbid type value))))
