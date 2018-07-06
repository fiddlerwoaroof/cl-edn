(in-package :edn)

(defun .satisfies (predicate &rest args)
  (.bind (.item)
         (lambda (x)
           (if (apply predicate x args)
               (.identity x)
               (.fail)))))

(defun .one-of (items &optional (test 'eql))
  (.satisfies
   (serapeum:op
     (member _ items :test test))))

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
  (.one-of '(#\space
             #\tab
             #\return
             #\newline
             #\,)))

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

(defun .compound-element-start ()
  (.or (.string= "#{")
       (.char= #\{)
       (.char= #\[)
       (.char= #\()
       (.char= #\{)))

(defun .compound-element-finish (closing)
  (lambda ()
    (.prog1 (.first (.elements))
            (.s)
            (.char= closing))))

(defun .map-element ()
  (.prog2 (.char= #\{)
          (.progn (.s)
                  (.zero-or-more (.let* ((first (.prog1 (.element) (.s)))
                                         (second (.prog1 (.element) (.s))))
                                   (.identity (list :pair first second)))))
          (.char= #\})))

(defun .between (start-parser end-parser element-parser)
  (.prog2 start-parser
          (.first element-parser)
          (.s)
          end-parser))

(defun .tag (tag parser)
  (.let* ((item parser))
    (.identity (cons tag item))))

(defun .collection (tag start-parser end-parser)
  (.tag tag
        (.between start-parser end-parser
                  (.elements))))

(defun .primitive ()
  (.or (.nil)
       (.boolean)))

(defun .collections ()
  (.alt (.tag :map (.map-element))
        (.collection :set (.string= "#{") (.char= #\}))
        (.collection :vector (.char= #\[) (.char= #\]))
        (.collection :list (.char= #\() (.char= #\)))))

(defun .atoms ()
  (.alt (.number)
        (.symbol)
        (.keyword)
        (.character)
        (.string)))

(defun .element ()
  (.or (.primitive)
       (.alt (.atoms)
             (.collections)
             
             (.tag :tagged
                   (.let* ((tag (.progn (.char= #\#) (.tag-symbol)))
                           (element (.progn (.s) (.element))))
                     (.identity (list tag element)))))))

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
                         (second (.satisfies (complement #'digit-char-p)))
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
(defun apply-sign (sign num)
  (if sign
      (ecase sign
        (#\+ num)
        (#\- (* -1 num)))
      num))

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
                   'double-float))
         0))))

(defun interpret-number (parts)
  (destructuring-bind (sign radix float-info flag) parts
    (let* ((base-value (if float-info
                           (destructuring-bind (mantissa exp) float-info
                             (coerce (* (+ radix
                                           (or mantissa 0))
                                        (if exp
                                            (expt 10 exp)
                                            1))
                                     'double-float))
                           radix))
           (signed (case sign
                     ((#\+ nil) base-value)
                     (#\- (- base-value)))))
      (typecase signed
        (integer (if (member flag '(nil #\N))
                     (.identity signed)
                     (.fail)))
        (float (if (member flag '(nil #\M))
                   (.identity signed)
                   (.fail)))))))

(defun .number ()
  (flet ((.sign () (.one-of '(#\+ #\-))))
    (.let* ((sign (.optional (.sign)))
            (num (.cardinal))
            (frac (.optional (.frac)))
            (exp (.optional (.exp)))
            (flag (.optional (.one-of '(#\N #\M)))))
      (interpret-number
       (list sign
             num
             (when (or frac exp)
               (list frac exp))
             flag)))))

(defun .exp ()
  (.progn (.char-equal #\e)
          (.let* ((sign (.optional
                         (.or (.char= #\+)
                              (.char= #\-))))
                  (num (.cardinal)))
            (.identity (apply-sign sign num)))))

(defun .cardinal ()
  (.let* ((nums (.or (.first
                      (.let* ((first (.non-zero-digit))
                              (rest (.zero-or-more
                                     (.digit))))
                        (.identity (list* first rest))))
                     (.let* ((c (.digit)))
                       (.identity (list c))))))
    (.identity (parse-integer (coerce nums 'string)))))

(defun .digit ()
  (.satisfies #'digit-char-p))

(defun .non-zero-digit ()
  (.satisfies (lambda (x)
                (and (digit-char-p x)
                     (not (eql #\0 x))))))

(defun .printable-character ()
  (.or (.satisfies (lambda (x) (char>= #\~ x #\!)))
       (.satisfies (lambda (x) (char<= #\space x)))))

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

(defun read-edn (s)
  (car
   (smug:parse (.prog1 (.elements)
                       (.s)
                       (.not (.item)))
               s)))

(defun parse (input &optional (realizer 'fset))
  (synthesize realizer
              (read-edn input)))
