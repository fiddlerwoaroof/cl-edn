(in-package :edn.generate)

(defun generate-edn-in-range (min-nodes max-nodes)
  (loop for (edn nodes) = (multiple-value-list (generate-edn))
     until (<= min-nodes nodes max-nodes)
     finally (return (values edn nodes))))

(defvar *last-edn*)
(defun generate-edn ()
  (multiple-value-bind (edn nodes) (%generate-edn)
    (values (setf *last-edn* edn)
            nodes)))

(defun last-generated ()
  (when (boundp '*last-edn*)
    *last-edn*))

(defun %generate-edn ()
  (ecase (random 3)
    (0 (generate-map))
    (1 (generate-set))
    (2 (generate-vect))))

(defun generate-nil ()
  (values "nil" 1))

(defun prim-generate-char ()
  (code-char (+ 32 (random #.(- 128 32)))))

(defun generate-string ()
  (values (loop with limit = (random 25)
             repeat limit
             collect (prim-generate-char) into chars
             finally (return (format nil "\"~a\""
                                     (serapeum:string-replace-all "\""
                                                                  (serapeum:string-replace-all
                                                                   "\\"
                                                                   (coerce chars 'string)
                                                                   "\\\\")
                                                                  "\\\""))))
          1))

(defun generate-int ()
  (values (princ-to-string (- (random 20000)
                              10000))
          1))

(defun flip-coin ()
  (= 1 (random 2)))

(defun generate-float ()
  (values (format nil "~[~;-~;+~]~a.~:[~;~:*~a~]~:[~;e~:*~a~]~:[~;M~]"
                  (random 3)
                  (if (flip-coin)
                      (random 10000)
                      0)
                  (when (flip-coin)
                    (random 10000))
                  (when (flip-coin)
                    (- (random 100)
                       50))
                  (flip-coin))
          1))

(defun generate-character ()
  (values (format nil "\\~c" (prim-generate-char))
          1))

(defun generate-bool ()
  (values (if (flip-coin)
              "true"
              "false")
          1))

(defmacro comment (&body b)
  (declare (ignore b))
  (format nil ";foobar~%"))

(comment
  (or (alpha-char-p x)
      (member x '(#\! #\* #\? #\_ #\$ #\% #\& #\=))))

(defun generate-capital ()
  (code-char
   (+ #.(char-code #\A)
      (random 26))))

(defun generate-lower ()
  (code-char
   (+ #.(char-code #\a)
      (random 26))))

(defun generate-initial-char ()
  (ecase (random 2)
    (0 (generate-capital))
    (1 (generate-lower))))

(defun generate-middle-char ()
  (ecase (random 5)
    (0 (generate-capital))
    (1 (generate-lower))
    (2 (generate-capital))
    (3 (generate-lower))
    (4 (elt #(#\- #\_) (random 2)))))

(defun generate-name (&optional (length 20))
  (loop repeat (+ 2 (random length))
     for char = (generate-initial-char) then (generate-middle-char)
     collect char into chars
     finally (return (coerce chars 'string))))

(defun generate-symbol ()
  (values (let ((ns (generate-name 5))
                (name (generate-name 20)))
            (if (flip-coin)
                name
                (format nil "~a/~a" ns name)))
          1))

(defun generate-keyword ()
  (values (format nil ":~a" (generate-symbol))
          1))

(defun generate-primitive ()
  (ecase (random 8)
    (0 (generate-string))
    (1 (generate-int))
    (2 (generate-bool))
    (3 (generate-float))
    (4 (generate-nil))
    (5 (generate-character))
    (6 (generate-keyword))
    (7 (generate-symbol))))

(defun compound-or-primitive (&optional (primitive-func 'generate-primitive))
  (ecase (random 10)
    (0 (%generate-edn))
    (1 (funcall primitive-func))
    (2 (funcall primitive-func))
    (3 (funcall primitive-func))
    (4 (funcall primitive-func))
    (5 (funcall primitive-func))
    (6 (funcall primitive-func))
    (7 (funcall primitive-func))
    (8 (funcall primitive-func))
    (9 (funcall primitive-func))))

(defun not-float ()
  (compound-or-primitive
   (lambda ()
     (ecase (random 5)
       (0 (generate-string))
       (1 (generate-int))
       (2 (generate-bool))
       (3 (generate-nil))
       (4 (generate-character))))))

(defun generate-map (&optional (key-func 'not-float) (value-func 'compound-or-primitive))
  (loop
     with nodes = 0
     with keys = (fset:set)
     repeat (random 10)
     for key = (loop for (next key-nodes) = (multiple-value-list (funcall key-func))
                  until (not (fset:contains? keys next))
                  do (incf nodes key-nodes)
                  finally
                    (fset:includef keys next)
                    (return next))
     for (value value-nodes) = (multiple-value-list (funcall value-func))
     do (incf nodes value-nodes)
     collect (format nil "~a ~a" key value) into res
     finally (return (values (format nil "{~{~{~a~^~[ ~;, ~;,~; ,~]~}~}}"
                                     (mapcar (serapeum:op (list _1 (random 3)))
                                             (remove-duplicates res :test 'equal)))
                             nodes))))


(defun generate-set (&optional (value-func 'not-float))
  (loop
     with nodes = 0
     repeat (random 19)
     for (value value-nodes) = (multiple-value-list (funcall value-func))
     collect value into res
     do (incf nodes value-nodes)
     finally (return (values (format nil "#{~{~{~a~^~[ ~;, ~;,~; ,~]~}~}}"
                                     (mapcar (serapeum:op (list _1 (random 3)))
                                             (remove-duplicates res :test 'equal)))
                             nodes))))

(defun generate-vect (&optional (value-func 'compound-or-primitive))
  (loop
     with nodes = 0
     repeat (random 19)
     for (value value-nodes) = (multiple-value-list (funcall value-func))
     collect value into res
     do (incf nodes value-nodes)
     finally (return (values (format nil "[~{~{~a~^~[ ~;, ~;,~; ,~]~}~}]"
                                     (mapcar (serapeum:op (list _1 (random 3)))
                                             res))
                             nodes))))
