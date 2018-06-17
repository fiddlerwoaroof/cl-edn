(defpackage :edn
  (:use :cl :smug)
  (:export ))
(defpackage :edn-primitives
  (:use)
  (:export :nil :true :false))

(defconstant edn-primitives:nil 'edn-primitives:nil)
(defconstant edn-primitives:true 'edn-primitives:true)
(defconstant edn-primitives:false 'edn-primitives:false)

(in-package :edn)

(defun .satisfies (predicate &rest args)
  (.bind (.item)
         (lambda (x)
           (if (apply predicate x args)
               (.identity x)
               (.fail)))))

(defun .zero-or-more (parser)
  (.plus (.let* ((x parser)
                 (xs (.zero-or-more parser)))
           (.identity (cons x xs)))
         (.identity ())))

(defun .one-or-more (parser)
  (.let* ((x parser)
          (y (.zero-or-more parser)))
    (.identity (cons x y))))

(defun .elements ()
  (.zero-or-more (.progn (.s)
                         (.element))))

(defun .s ()
  (.zero-or-more
   (.or (.whitespace)
        (.comment)
        (.discarded-element))))

(defun .whitespace ()
  (.or (.char= #\space)
       (.char= #\tab)
       (.char= #\return)
       (.char= #\newline)
       (.char= #\,)))

(defun .comment ()
  (.let* ((result (.prog2 (.char= #\;)
                          (.zero-or-more (.and (.not (.or (.char= #\newline)
                                                          (.char= #\nul)))
                                               (.item)))
                          (.or (.char= #\newline)
                               (.not (.item))))))
    (.identity (list :comment (coerce result 'string)))))

(defun .discarded-element ()
  (.progn (.string= "#_")
          (.s)
          (.element)))

(defun .alt (&rest r)
  (reduce '.plus r))

(defun .element ()
  (.or (.or (.nil)
            (.boolean))
       (.alt (.symbol)
             (.keyword)
             (.number)
             (.character)
             (.string)
             
             (.let* ((pairs
                      (.prog2 (.char= #\{)
                              (.progn (.s)
                                      (.zero-or-more (.let* ((first (.prog1 (.element) (.s)))
                                                             (second (.prog1 (.element) (.s))))
                                                       (.identity (list :pair first second)))))
                              (.char= #\}))))
               (.identity (cons :map pairs)))
             (.let* ((pairs
                      (.prog2 (.string= "#{")
                              (.elements)
                              (.s)
                              (.char= #\}))))
               (.identity (cons :set pairs)))
             (.let* ((pairs
                      (.prog2 (.char= #\[)
                              (.elements)
                              (.s)
                              (.char= #\]))))
               (.identity (cons :vector pairs)))
             (.let* ((pairs
                      (.prog2 (.char= #\()
                              (.elements)
                              (.s)
                              (.char= #\)))))
               (.identity (cons :list pairs)))
             (.let* ((tag (.progn (.char= #\#) (.tag-symbol)))
                     (element (.progn (.s) (.element))))
               (.identity (list :tagged tag element)))
             )))

(defun .nil ()
  (.and (.string= "nil")
        (.identity edn-primitives:nil)))

(defun .boolean ()
  (.let* ((r (.or (.string= "true")
                  (.string= "false"))))
    (string-case:string-case (r)
      ("true" (.identity edn-primitives:true))
      ("false" (.identity edn-primitives:false)))))

(defun .symbol ()
  (.plus (.char= #\/)
         (.let* ((ns (.optional (.prog1 (.name) (.char= #\/))))
                 (name (.name)))
           (.identity (list :symbol ns name)))))

(defun .tag-symbol ()
  (.let* ((first (.satisfies #'alpha-char-p))
          (rest (.let* ((ns (.or (.char= #\/)
                                 (.optional (.prog1 (.name) (.char= #\/)))))
                        (name (.name)))
                  (.identity (list ns name)))))
    (destructuring-bind (ns name) rest
      (if ns
          (if (eql ns #\/)
              (.identity (list :symbol (format nil "~c" first) name))
              (.identity (list :symbol (format nil "~c~a" first ns) name)))
          (.identity (list :symbol nil (format nil "~c~a" first name)))))))

(defun .keyword ()
  (.progn (.char= #\:)
          (.let* ((ns (.optional (.prog1 (.name) (.char= #\/))))
                  (name (.name)))
            (.identity (list :keyword ns name)))))

(defun .name ()
  (.first (.plus (.let* ((first (.name-start-1))
                         (rest (.zero-or-more (.name-constituent))))
                   (.identity (format nil "~c~{~c~}" first rest)))
                 (.let* ((first (.name-start-2))
                         (second (.satisfies #'alpha-char-p))
                         (rest (.zero-or-more (.name-constituent))))
                   (.identity (format nil "~c~c~{~c~}" first second rest))))))

(defun .name-start-1 ()
  (.satisfies (lambda (x)
                (or (alpha-char-p x)
                    (member x '(#\! #\* #\? #\_ #\$ #\% #\& #\=))))))

(defun .name-start-2 ()
  (.satisfies (lambda (x)
                (or (alpha-char-p x)
                    (member x '(#\. #\- #\+))))))

(defun .name-constituent ()
  (.or (.name-start-1)
       (.name-start-2)
       (.satisfies (lambda (x)
                     (or (digit-char-p x)
                         (member x '(#\# #\:)))))))
(defun .number ()
  (.or (.float)
       (.integer)))

(defun apply-sign (sign num)
  (if sign
      (ecase sign
        (#\+ num)
        (#\- (* -1 num)))
      num))

(defun .integer ()
  (.let* ((sign (.optional
                 (.or (.char= #\+)
                      (.char= #\-))))
          (num (.cardinal))
          (flag (.optional (.char= #\N))))
    flag
    (.identity (apply-sign sign num))))

(defun .float ()
  (.let* ((sign (.optional
                 (.or (.char= #\+)
                      (.char= #\-))))
          (num (.cardinal))
          (frac (.frac-exp)))
    (destructuring-bind (mant exp) frac
      (.identity (apply-sign sign (* (+ num mant)
                                     (if exp
                                         (expt 10 exp)
                                         1)))))))

(defun .frac-exp ()
  (.alt (.let* ((frac (.frac))
                (exp (.optional (.exp)))
                (flag (.optional (.char= #\M))))
          flag
          (.identity (list frac exp)))
        (.let* ((exp (.exp))
                (flag (.optional (.char= #\M))))
          flag
          (.identity (list 0 exp)))
        (.let* ((flag (.optional (.char= #\M))))
          flag
          (.identity (list 0 0)))))

(defun .frac ()
  (declare (optimize debug))
  (.let* ((nums (.first
                 (.progn (.char= #\.)
                         (.zero-or-more (.digit))))))
    (.identity
     (if nums
         (let ((num (parse-integer (coerce nums 'string))))
           (coerce (if (= num 0)
                       0
                       (/ num
                          (expt 10
                                (floor
                                 (1+ (log num
                                          10))))))
                   'float))
         0))))

(defun .exp ()
  (.progn (.char-equal #\e)
          (.let* ((sign (.optional
                         (.or (.char= #\+)
                              (.char= #\-))))
                  (num (.cardinal)))
            (.identity (apply-sign sign num)))))

(defun .cardinal ()
  (.let* ((nums (.first
                 (.or (.let* ((first (.non-zero-digit))
                              (rest (.zero-or-more (.digit))))
                        (.identity (list* first rest)))
                      (.let* ((c (.digit)))
                        (.identity (list c)))))))
    (.identity (parse-integer (coerce nums 'string)))))

(defun .digit ()
  (.satisfies #'digit-char-p))

(defun .non-zero-digit ()
  (.satisfies (lambda (x)
                (and (digit-char-p x)
                     (not (eql #\0 x))))))

(defun .printable-character ()
  (.or (.satisfies (lambda (x) (char>= #\~ x #\!)))
       (.satisfies (lambda (x) (>= (char-code x) #xA1)))))

(defun .character-name ()
  (.or (.string= "newline")
       (.string= "space")
       (.string= "tab")
       (.string= "return")
       (.string= "backspace")
       (.string= "formfeed")))

(defun .character ()
  (.let* ((char (.progn (.char= #\\)
                        (.or (.character-name)
                             (.printable-character)))))
    (.identity (list :character char))))

(defun .string-char ()
  (.and (.not (.char= #\nul))
        (.not (.char= #\"))
        (.not (.char= #\\))
        (.item)))

(defun .string-escape ()
  (.let* ((esc (.or (.char= #\")
                    (.char= #\b)
                    (.char= #\t)
                    (.char= #\n)
                    (.char= #\f)
                    (.char= #\r)
                    (.char= #\\))))
    (.identity (format nil "\\~c" esc))))

(defun combine (list)
  (format nil "~{~a~}" list))

(defun .string ()
  (.let* ((string (.prog2 (.char= #\")
                          (.zero-or-more (.or (.string-char)
                                              (.progn (.char= #\\)
                                                      (.string-escape))))
                          (.char= #\"))))
    (.identity (list :string (combine string)))))

(defgeneric synthesize-compound (implementation discriminator args))
(defgeneric synthesize (implementation args))
