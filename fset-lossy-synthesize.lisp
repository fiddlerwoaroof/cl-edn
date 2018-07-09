;; capitalizes symbols ---- possible information loss
(in-package :edn)

(defclass fset-lossy (fset)
  ()
  (:documentation "An eden synthesizer that applies semantic
  transformations that could lose information: in particular, when
  it's translating a keyword or symbol, it uppercases the namespace
  and name to match CL's symbol behavior"))


(defmethod synthesize-compound ((implementation fset-lossy) (discriminator (eql :list)) args)
  (mapcar (lambda (a)
            (synthesize implementation a))
          args))

(defmethod synthesize-compound :around ((implementation fset-lossy) (discriminator (eql :keyword)) args)
  (destructuring-bind (ns name) args
    (call-next-method implementation
                      discriminator
                      (list (when ns (string-upcase ns))
                            (string-upcase name)))))

(defmethod synthesize-compound (implementation (discriminator (eql :string)) args)
  (car args))

(defmethod synthesize-compound :around ((implementation fset-lossy) (discriminator (eql :symbol)) args)
  (destructuring-bind (ns name) args
    (call-next-method implementation
                      discriminator
                      (list (when ns (string-upcase ns))
                            (string-upcase name)))))

