(in-package :edn)

(defparameter *name-start-chars*
  (concatenate 'list
               "qwertyuiopasdfghjklzxcvbnm"
               "QWERTYUIOPASDFGHJKLZXCVBNM"
               '(#\! #\* #\? #\_ #\$ #\% #\& #\=)))

(defun read-char-consuming-whitespace (s _ __)
  (declare (ignore _ __))
  (loop for next-char = (read-char s nil 'eof)
        when (eql next-char 'eof)
          do (return-from read-char-consuming-whitespace 'eof)
        while (whitespacep next-char)
        finally (return next-char)))

(defun read-while (pred s eof-error-p eof-value)
  (coerce (loop for char = (read-char s eof-error-p eof-value)
                until (eq char eof-value)
                while (funcall pred char)
                collect char into result
                finally (unread-char char s)
                        (if (and (eq char eof-value)
                                 (null result))
                            (return-from read-while eof-value)
                            (return result)))
          'string))

(defun read-comment (s node-handler dispatch trigger-char)
  (declare (ignore dispatch trigger-char))
  (handle-parse-event node-handler :comment (read-line s nil "")))

(defun whitespacep (c)
  (case c
    ((#\space #\tab #\newline #\return #\,) t)
    (t nil)))

(defun unclosed-error (s node-handler dispatch trigger-char)
  (declare (ignore s dispatch node-handler))
  (error "unexpected ~c, unclosed expression" trigger-char))

(defun number-constituent-p (c)
  (or (digit-char-p c 10)
      (case c
        ((#\. #\e #\/ #\N #\M #\x #\X) t))))

(defun read-number (s node-handler dispatch trigger-digit)
  (declare (ignore dispatch))
  (unless (member trigger-digit '(#\- #\+))
    (unread-char trigger-digit s))
  (let* ((sign (car (member trigger-digit '(#\- #\+))))
         (num (read-while #'digit-char-p s nil 'eof))
         (frac (let ((c (read-char s nil 'eof)))
                 (if (eql #\. c)
                     (read-while #'digit-char-p s nil 'eof)
                     (progn (unread-char c s)
                            nil))))
         (exp (let ((c (read-char s nil 'eof)))
                (if (eql #\e c)
                    (parse-integer
                     (read-while (data-lens:disj
                                  (data-lens:== #\+)
                                  (data-lens:== #\-)
                                  #'digit-char-p)
                                 s
                                 nil
                                 'eof))
                    (progn (unread-char c s)
                           nil))))
         (flag (when (member (peek-char nil s nil 'eof)
                             '(#\N #\M))
                 (read-char s nil 'eof)))
         (result
           (interpret-number (list sign
                                   ;; TODO: handle hex and octal
                                   (if (equal num "")
                                       0
                                       (parse-integer num))
                                   ;; TODO: handle leading 0s
                                   (when (or (and frac (not (eql frac 'eof)))
                                             (and exp (not (eql exp 'eof))))
                                     (list (parse-frac frac)
                                           exp))
                                   flag))))
    (handle-parse-event node-handler
                        :number
                        result)))

(defun read-symbol (s node-handler dispatch trigger-char)
  (declare (ignore dispatch))
  (let ((read (list* trigger-char
                     (loop for next-char = (read-char s nil 'eof)
                           until (eql next-char 'eof)
                           while (or (eql next-char #\/)
                                     (name-constituent-p next-char))
                           collect next-char
                           finally (unread-char next-char s)))))
    (destructuring-bind (ns name) (fwoar.string-utils:partition #\/ (coerce read 'string))
      (when (null name)
        (rotatef ns name))
      (if (null ns)
          (string-case:string-case (name)
            ("nil" edn-primitives:nil)
            ("true" edn-primitives:true)
            ("false" edn-primitives:false)
            (t (list :symbol ns name)))
          (handle-parse-event node-handler :symbol name :namespace ns)))))

(defun read-keyword (s node-handler dispatch trigger-char)
  (declare (ignore trigger-char))
  (let ((next-char (read-char s nil 'eof)))
    (cond
      ((eql next-char 'eof)
       (error "bad keyword"))
      (t
       (destructuring-bind (_ ns name) (read-symbol s node-handler dispatch next-char)
         (declare (ignore _))
         (list :keyword ns name))))))

(defun read-number-or-symbol (s node-handler dispatch trigger-char)
  (let ((next (peek-char nil s nil 'eof)))
    (cond
      ((and (not (eql next 'eof))
            (digit-char-p next))
       (read-number s node-handler dispatch trigger-char))
      (t
       (read-symbol s node-handler dispatch trigger-char)))))

(defun read-paren (s node-handler dispatch trigger-char)
  (declare (ignore trigger-char))
  (let ((list-id (gensym "LIST")))
    (handle-parse-event node-handler :start-list list-id)
    (loop for next-char = (read-char-consuming-whitespace s nil 'eof)
          until (eql next-char #\))
          when (eql next-char 'eof) do
            (return-from read-paren 'eof)
          do (unread-char next-char s)
             (handle-parse-event node-handler
                                 :list-item
                                 (do-read s node-handler dispatch)
                                 :id list-id))
    (handle-parse-event node-handler :end-list list-id)))

(defun read-square (s node-handler dispatch trigger-char)
  (declare (ignore trigger-char))
  (let ((vector-id (gensym "VECTOR")))
    (handle-parse-event node-handler :start-vector vector-id)
    (loop for next-char = (read-char-consuming-whitespace s nil 'eof)
          until (eql next-char #\])
          when (eql next-char 'eof) do
            (return-from read-square 'eof)
          do (unread-char next-char s)
             (handle-parse-event node-handler
                                 :vector-item
                                 (do-read s node-handler dispatch)
                                 :id vector-id))
    (handle-parse-event node-handler :end-vector vector-id)))

(defun read-curly (s node-handler dispatch trigger-char)
  (declare (ignore trigger-char))
  (let ((map-id (gensym "MAP")))
    (handle-parse-event node-handler :start-map map-id)
    (loop for next-char = (read-char-consuming-whitespace s nil 'eof)
          until (eql next-char #\})
          when (eql next-char 'eof) do
            (return-from read-curly 'eof)
          do (unread-char next-char s)
             (handle-parse-event node-handler
                                 :map-item
                                 (do-read s node-handler dispatch)
                                 :id map-id
                                 :value (do-read s node-handler dispatch)))
    (handle-parse-event node-handler :end-map map-id)))


(defun read-string (s node-handler dispatch trigger-char)
  (declare (ignore dispatch trigger-char))
  (handle-parse-event node-handler
                      :string
                      (translate-escapes
                       (coerce (loop for next-char = (read-char s nil 'eof)
                                     until (or (eql next-char 'eof)
                                               (eql next-char #\"))
                                     collect next-char)
                               'string))))

(defun read-set (s node-handler dispatch trigger-char)
  (declare (ignore dispatch trigger-char))
  (let ((set-id (gensym "SET")))
    (handle-parse-event node-handler :start-set set-id)
    (loop for next-char = (read-char-consuming-whitespace s nil 'eof)
          until (eql next-char #\})
          when (eql next-char 'eof) do
            (return-from read-set 'eof)
          do (unread-char next-char s)
             (handle-parse-event node-handler
                                 :set-item
                                 (do-read s node-handler *dispatch*)
                                 :id set-id))
    (handle-parse-event node-handler :end-set set-id)))

(defun read-tagged (s node-handler dispatch trigger-char)
  (let ((tag (read-symbol s node-handler dispatch trigger-char))
        (datum (do-read s node-handler *dispatch*)))
    (handle-parse-event node-handler
                        :tag
                        tag)
    (handle-parse-event node-handler
                        :tagged
                        datum
                        :tag tag)))


(defun read-character (s node-handler dispatch trigger-char)
  (declare (ignore dispatch trigger-char))
  (flet ((handle-character-name (name)
           (string-case:string-case (name :default (if (= 1 (length name))
                                                       name
                                                       (error "invalidate character name ~s" name)))
             ("backspace" #\backspace)
             ("formfeed" #.(code-char 12))
             ("newline" #\newline)
             ("return" #\return)
             ("space" #\space)
             ("tab" #\tab))))
    (let ((name (coerce (loop for char = (read-char s nil 'eof)
                              for first-char = t then nil
                              until (or (eql char 'eof)
                                        (whitespacep char)
                                        (unless first-char
                                          (unless (alpha-char-p char)
                                            (unread-char char s)
                                            t)))
                              collect char)
                        'string)))
      (handle-parse-event node-handler
                          :character
                          (handle-character-name name)))))

(defclass collecting-handler ()
  ((collectors :reader collectors :initform (make-hash-table
                                             #+sbcl #+sbcl
                                             :weakness :key))))
(defclass printing-handler ()
  ((collectors :reader collectors :initform (make-hash-table
                                             #+sbcl #+sbcl
                                             :weakness :key))))
(defvar *handler* nil)

(defgeneric handle-parse-event (handler event datum &key &allow-other-keys)
  #+(or)
  (:method :around (handler event datum &rest r &key)
    (:printv (list handler event datum r))
    (:printv (call-next-method)))
  (:method :before ((handler printing-handler) event datum &rest r &key)
    (etypecase datum
      (number (pprint (list* event datum r)))
      (t (pprint (list* event datum r)))))
  (:method ((handler printing-handler) event datum &key)
    `(:ref ,(sxhash datum)))
  (:method ((handler printing-handler) (event (eql :number)) datum &key)
    datum)
  (:method ((handler printing-handler) (event (eql :symbol)) datum &key)
    datum)
  (:method ((handler printing-handler) (event (eql :tagged)) datum &key)
    datum)
  (:method ((handler printing-handler) (event (eql :tagged)) datum &key)
    datum)

  #+(or)
  (:method ((handler null) event datum &rest r &key)
    (apply #'handle-parse-event
           (make-instance 'collecting-handler)
           event
           datum
           r))
  (:method ((handler collecting-handler) event datum &key)
    (list event datum))

  (:method ((handler collecting-handler) (event (eql :number)) datum &key)
    datum)
  (:method ((handler collecting-handler) (event (eql :symbol)) datum &key namespace)
    (list :symbol namespace datum))
  (:method ((handler collecting-handler) (event (eql :tagged)) datum &key tag)
    (list :tagged tag datum))
  (:method ((handler collecting-handler) (event (eql :tagged)) datum &key tag)
    (list :tagged tag datum))

  (:method ((handler collecting-handler) (event (eql :start-set)) datum &key)
    (setf (gethash datum (collectors handler)) ()))
  (:method ((handler collecting-handler) (event (eql :set-item)) datum &key id)
    (push datum
          (gethash id (collectors handler))))
  (:method ((handler collecting-handler) (event (eql :end-set)) datum &key)
    `(:set ,@(nreverse (gethash datum (collectors handler)))))

  (:method ((handler collecting-handler) (event (eql :start-vector)) datum &key)
    (setf (gethash datum (collectors handler)) ()))
  (:method ((handler collecting-handler) (event (eql :vector-item)) datum &key id)
    (push datum
          (gethash id (collectors handler))))
  (:method ((handler collecting-handler) (event (eql :end-vector)) datum &key)
    `(:vector ,@(nreverse (gethash datum (collectors handler)))))

  (:method ((handler collecting-handler) (event (eql :start-list)) datum &key)
    (setf (gethash datum (collectors handler)) ()))
  (:method ((handler collecting-handler) (event (eql :list-item)) datum &key id)
    (push datum
          (gethash id (collectors handler))))
  (:method ((handler collecting-handler) (event (eql :end-list)) datum &key)
    `(:list ,@(nreverse (gethash datum (collectors handler)))))

  (:method ((handler collecting-handler) (event (eql :start-map)) datum &key)
    (setf (gethash datum (collectors handler)) ()))
  (:method ((handler collecting-handler) (event (eql :map-item)) datum &key id value)
    (push `(:pair ,datum ,value)
          (gethash id (collectors handler))))
  (:method ((handler collecting-handler) (event (eql :end-map)) datum &key)
    `(:map ,@(nreverse (gethash datum (collectors handler)))))

  )

(defparameter *hash-dispatch*
  (alexandria:alist-hash-table
   (append (loop for char in *name-start-chars*
                 collect (cons char 'read-tagged))
           (loop for char across "1234567890"
                 collect (cons char 'read-number))
           '((#\{ . read-set)
             (#\} . unclosed-error)))))

(defun read-hash (s node-handler dispatch trigger-char)
  (declare (ignore dispatch trigger-char))
  (do-read s node-handler *hash-dispatch*))


(defun do-read (s node-handler &optional (dispatch *dispatch*))
  (declare (inline))
  (let* ((next-char (read-char-consuming-whitespace s nil 'eof))
         (handler (gethash next-char dispatch)))
    (cond ((eql next-char 'eof)
           'eof)
          (handler
           (funcall handler s node-handler dispatch next-char))
          ((whitespacep next-char)
           (do-read s node-handler dispatch))
          (t
           (error "no dispatch for ~c" next-char)))))


(defparameter *dispatch*
  (alexandria:alist-hash-table
   (append (loop for char in *name-start-chars*
                 collect (cons char 'read-symbol))
           (loop for char across "1234567890"
                 collect (cons char 'read-number))
           '((#\( . read-paren)
             (#\) . unclosed-error)
             (#\{ . read-curly)
             (#\} . unclosed-error)
             (#\[ . read-square)
             (#\] . unclosed-error)
             (#\: . read-keyword)
             (#\; . read-comment)
             (#\" . read-string)
             (#\\ . read-character)
             (#\- . read-number-or-symbol)
             (#\+ . read-number-or-symbol)
             (#\. . read-number-or-symbol)
             (#\# . read-hash)))))

(defgeneric parse-edn-new (it)
  (:method ((input string))
    (fw.lu:closing
        (parse-edn-new (make-string-input-stream input))))
  (:method ((input stream))
    (let ((handler (make-instance 'collecting-handler)))
      (values (do-read input (or *handler* handler))
              (collectors handler))))
  (:method ((input pathname))
    (fw.lu:closing
        (parse-edn-new (open input)))))
