(in-package :edn)

(defun .0-or-more (parser)
  (lambda (input)
    (loop
       for remaining-input = input then (cdr result)
       for result = (first (funcall parser remaining-input))
       while (and (car result) (> (length remaining-input) 0))
       collect (car result) into matches
       finally (return (list (cons matches remaining-input))))))

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

(defun .elements ()
  (.0-or-more (.progn (.s)
                      (.element))))

(defun .s ()
  (.first
   (.0-or-more
    (.or (.whitespace)
         (.comment)
         (.discarded-element)))))

(defun .whitespace ()
  (.one-of '(#\space
             #\tab
             #\return
             #\newline
             #\,)))

(defmacro read-if (s test)
  `(when (funcall (lambda (_)
                    ,test)
                  (peek-char nil ,s))
     (read-char s)))

(defun parse-whitespace (s)
  (read-if s (member _ '(#\space #\, #\tab #\return #\newline))))

(defun .comment ()
  (.let* ((result (.prog2 (.char= #\;)
                          (.first
                           (.0-or-more
                            (.and (.not (.or (.char= #\newline)
                                             (.char= #\nul)))
                                  (.item))))
                          (.or (.char= #\newline)
                               (.not (.item))))))
    (.identity (list :comment (coerce result 'string)))))

(defun .discarded-element ()
  (.progn (.string= "#_")
          (.element)))

(defun .alt (&rest r)
  (reduce '.plus r))

(defun .map-element ()
  (.prog2 (.char= #\{)
          (.progn (.s)
                  (.first
                   (.0-or-more (.let* ((first (.prog1 (.element) (.s)))
                                          (second (.prog1 (.element) (.s))))
                                    (.identity (list :pair first second))))))
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

(defun .juxt (a b)
  (.let* ((first a)
          (second b))
    (.identity (list first second))))

(defun .name ()
  (.let* ((prefix (.or (.let* ((first (.name-start-1)))
                         (.identity (string first)))
                       (.let* ((first (.juxt (.name-start-2)
                                             (.satisfies (complement #'digit-char-p)))))
                         (.identity (coerce first 'string)))))
          (suffix (.0-or-more (.name-constituent))))
    (.identity (concatenate 'string prefix suffix))))

(defun name-start-1-p (c)
  (member c
          '(#\! #\* #\? #\_
            #\$ #\% #\& #\=)))

(defun name-start-2-p (c)
  (member c '(#\. #\- #\+)))

(defun .name-start-1 ()
  (.or (.satisfies 'alpha-char-p)
       (.one-of '(#\! #\* #\? #\_ #\$ #\% #\& #\=))))

(defun .name-start-2 ()
  (.one-of '(#\. #\- #\+)))

(defun name-constituent-p (c)
  (or (alpha-char-p c)
      (digit-char-p c)
      (name-start-1-p c)
      (name-start-2-p c)
      (member c '(#\# #\:))))

(defun .name-constituent ()
  (.satisfies 'name-constituent-p))

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

(defun parse-frac (nums)
  (let ((num (parse-integer nums)))
    (coerce (if (= num 0)
                0
                (/ num
                   (expt 10
                         (length nums))))
            'double-float)))

(defun .frac ()
  (.let* ((nums (.first
                 (.progn (.char= #\.)
                         (.0-or-more (.digit))))))
    (.identity
     (if nums
         (parse-frac (coerce nums 'string))
         0))))

(defun interpret-number (parts)
  (destructuring-bind (sign radix float-info flag) parts
    (let* ((base-value (if float-info
                           (destructuring-bind (mantissa exp) float-info
                             (coerce (* (+ radix
                                            (or mantissa 0.0))
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
                     signed
                     (coerce signed 'double-float)))
        (float (if (member flag '(nil #\M))
                   signed
                   (throw 'fail nil)))))))

(defun .number ()
  (flet ((.sign () (.one-of '(#\+ #\-))))
    (.let* ((sign (.optional (.sign)))
            (num (.cardinal))
            (frac (.optional (.frac)))
            (exp (.optional (.exp)))
            (flag (.optional (.one-of '(#\N #\M)))))
      (let ((result (catch 'fail
                      (.identity
                       (interpret-number
                        (list sign
                              num
                              (when (or frac exp)
                                (list frac exp))
                              flag))))))
        (if result
            result
            (.fail))))))

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
                              (rest (.0-or-more
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

(defun translate-escape (c)
  (ecase c
    ((#\" #\\) c)
    (#\t #\tab)
    (#\n #\newline)
    (#\r #\return)
    (#\b #\backspace)
    (#\f #.(code-char 12))))

(defun parse-string-ending-old (s)
  (let ((pos 0)
        (done nil))
    (flet ((consume-char ()
             (prog1 (elt s pos)
               (setf done (= pos (length s)))
               (incf pos))))
      (let ((result (loop
                      for char = (serapeum:case-let (next (consume-char))
                                   (#\\ (translate-escape (consume-char)))
                                   (#\" nil)
                                   (t next))
                      while char
                      when (= pos (length s)) do (return nil)
                        collect char)))
        (if result
            (values (coerce result 'string) pos)
            (values nil 0))))))

(defun translate-escapes (s)
  (let ((parts (coerce (fwoar.string-utils:split #\\ s) 'list)))
    (serapeum:string-join (list* (car parts)
                                 (mapcan (lambda (part)
                                           (list (translate-escape (elt part 0))
                                                 (subseq part 1)))
                                         (cdr parts)))
                          "")))

(defun parse-string-ending (s)
  (declare (optimize (speed 3))
           (type simple-string s))
  (loop
     for possible-quote = (position #\" s) then (position #\" s
                                                          :start (1+ possible-quote))

     while possible-quote
     when (not (char= #\\ (aref s (1- possible-quote)))) do
       (return (values (translate-escapes (subseq s 0 possible-quote))
                       (1+ possible-quote)))))

(defun combine (list)
  (format nil "~{~a~}" list))

(define-condition invalid-string-ending (error)
  ())

(defun .string ()
  (.let* ((string (.prog2 (.char= #\")
                          (.first
                           (.0-or-more (.or (.string-char)
                                            (.let* ((escape-char (.progn (.char= #\\)
                                                                         (.string-escape))))
                                              (.identity (translate-escape escape-char))))))
                          (.char= #\"))))
    (.identity (list :string (combine string)))))

(defun .string-ending ()
  (lambda (input)
    (multiple-value-bind (ending count) (parse-string-ending input)
      (if (> count 0)
          (list (cons ending
                      (subseq input count)))
          nil))))

(defun .string.old ()
  (.let* ((string (.progn (.char= #\")
                          (.string-ending))))
    (.identity (list :string string))))

(defun read-edn (s)
  (car
   (smug:parse (.prog1 (.elements)
                       (.s)
                       (.not (.item)))
               s)))
