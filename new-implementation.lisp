(in-package :edn)

(defparameter *name-start-chars*
  (concatenate 'list
               "qwertyuiopasdfghjklzxcvbnm"
               "QWERTYUIOPASDFGHJKLZXCVBNM"
               '(#\! #\* #\? #\_ #\$ #\% #\& #\=)))

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

(defparameter *hash-dispatch*
  (alexandria:alist-hash-table
   (append (loop for char in *name-start-chars*
                 collect (cons char 'read-tagged))
           (loop for char across "1234567890"
                 collect (cons char 'read-number))
           '((#\{ . read-set)
             (#\} . unclosed-error)))))

(defun read-hash (s dispatch trigger-char)
  (declare (ignore dispatch trigger-char))
  (do-read s *hash-dispatch*))

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

(defun read-comment (s dispatch trigger-char)
  (declare (ignore dispatch trigger-char))
  (list :comment (read-line s nil "")))

(defun whitespacep (c)
  (case c
    ((#\space #\tab #\newline #\return #\,) t)
    (t nil)))

(defun do-read (s &optional (dispatch *dispatch*))
  (declare (inline))
  (let* ((next-char (read-char-consuming-whitespace s nil 'eof))
         (handler (gethash next-char dispatch)))
    (cond ((eql next-char 'eof)
           'eof)
          (handler
           (funcall handler s dispatch next-char))
          ((whitespacep next-char)
           (do-read s dispatch))
          (t
           (error "no dispatch for ~c" next-char)))))

(defun unclosed-error (s dispatch trigger-char)
  (declare (ignore s dispatch))
  (error "unexpected ~c, unclosed expression" trigger-char))

(defun number-constituent-p (c)
  (or (digit-char-p c 10)
      (case c
        ((#\. #\e #\/ #\N #\M #\x #\X) t))))

(defun read-number (s dispatch trigger-digit)
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
                 (read-char s nil 'eof))))
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

(defun read-symbol (s dispatch trigger-char)
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
          (list :symbol ns name)))))

(defun read-keyword (s dispatch trigger-char)
  (declare (ignore trigger-char))
  (let ((next-char (read-char s nil 'eof)))
    (cond
      ((eql next-char 'eof)
       (error "bad keyword"))
      (t
       (destructuring-bind (_ ns name) (read-symbol s dispatch next-char)
         (declare (ignore _))
         (list :keyword ns name))))))

(defun read-number-or-symbol (s dispatch trigger-char)
  (let ((next (peek-char nil s nil 'eof)))
    (cond
      ((and (not (eql next 'eof))
            (digit-char-p next))
       (read-number s dispatch trigger-char))
      (t
       (read-symbol s dispatch trigger-char)))))

(defun read-paren (s dispatch trigger-char)
  (declare (ignore trigger-char))
  (list* :list
         (loop for next-char = (read-char-consuming-whitespace s nil 'eof)
               until (eql next-char #\))
               when (eql next-char 'eof) do
                 (return-from read-paren 'eof)
               do (unread-char next-char s)
               collect (do-read s dispatch))))

(defun read-square (s dispatch trigger-char)
  (declare (ignore trigger-char))
  (list* :vector
         (loop for next-char = (read-char-consuming-whitespace s nil 'eof)
               until (eql next-char #\])
               when (eql next-char 'eof) do
                 (return-from read-square 'eof)
               do (unread-char next-char s)
               collect (do-read s dispatch))))

(defun read-curly (s dispatch trigger-char)
  (declare (ignore trigger-char))
  (list* :map
         (loop for next-char = (read-char-consuming-whitespace s nil 'eof)
               until (eql next-char #\})
               when (eql next-char 'eof) do
                 (return-from read-curly 'eof)
               do (unread-char next-char s)
               collect (do-read s dispatch) into keys
               collect (do-read s dispatch) into values
               finally (return (mapcar (lambda (k v)
                                         `(:pair ,k ,v))
                                       keys
                                       values)))))

(defun read-string (s dispatch trigger-char)
  (declare (ignore dispatch trigger-char))
  ;; TODO: handle escapes
  (coerce (loop for next-char = (read-char s nil 'eof)
                until (or (eql next-char 'eof)
                          (eql next-char #\"))
                collect next-char)
          'string))

(defun read-set (s dispatch trigger-char)
  (declare (ignore dispatch trigger-char))
  (list* :set
         (loop for next-char = (read-char-consuming-whitespace s nil 'eof)
               until (eql next-char #\})
               when (eql next-char 'eof) do
                 (return-from read-set 'eof)
               do (unread-char next-char s)
               collect (do-read s *dispatch*))))

(defun read-tagged (s dispatch trigger-char)
  (list :tagged
        (read-symbol s dispatch trigger-char)
        (do-read s *dispatch*)))


(defun read-character (s dispatch trigger-char)
  (declare (ignore dispatch trigger-char))
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
    (list :character
          (string-case:string-case (name :default (if (= 1 (length name))
                                                      (string name)
                                                      (error "invalidate character name ~s" name)))
            ("backspace" name)
            ("formfeed" name)
            ("newline" name)
            ("return" name)
            ("space" name)
            ("tab" name)))))

(defgeneric parse-edn-new (it)
  (:method ((input string))
    (fw.lu:closing
        (parse-edn-new (make-string-input-stream input))))
  (:method ((input stream))
    (do-read input)))
