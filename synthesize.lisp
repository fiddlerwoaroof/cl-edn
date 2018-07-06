(in-package :edn)


(defgeneric convert-primitive (implementation primitive))

(defgeneric synthesize (implementation args))
(defgeneric synthesize-compound (implementation discriminator args))

(defmethod synthesize ((implementation symbol) discriminator)
  (synthesize (make-instance 'implementation) discriminator))

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
    (list :tagged
          (synthesize-compound implementation (car sym) (cdr sym))
          (synthesize implementation obj))))

(defmethod synthesize-compound (implementation (discriminator (eql :character)) args)
  (car args))

(defmethod synthesize-compound (implementation (discriminator (eql :tagged)) args)
  (destructuring-bind (sym obj) args
    (let ((tag (synthesize-compound implementation (car sym) (cdr sym))))
      (alexandria:switch ((symbol-name tag) :test 'string-equal)
        ("inst" (local-time:parse-rfc3339-timestring (synthesize implementation obj)))
        ("uuid" (uuid:make-uuid-from-string (synthesize implementation obj)))
        (t (list :tagged tag
               (synthesize implementation obj)))))))
