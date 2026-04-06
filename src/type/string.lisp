(in-package #:binstruct)

(defparser base-char ()
  (for ((code (unsigned-byte 8)))
    (code-char code)))

(declaim (ftype (function (list) (values (simple-array base-char (*)))) bytes-base-string))
(defun bytes-base-string (list)
  (loop :for previous := nil :then current
        :for current :on list
        :for code :of-type (unsigned-byte 8) := (car current)
        :when (= code #x00)
          :return (if previous
                      (prog2 (setf (cdr previous) nil)
                          (coerce list '(simple-array base-char (*)))
                        (setf (cdr previous) current))
                      #.(coerce "" 'simple-base-string))
        :do (setf (car current) (code-char code))
        :finally (return (coerce list '(simple-array base-char (*))))))

(defparser simple-base-string/fixed-length (size)
  (for ((list (rep (unsigned-byte 8) size size)))
    (bytes-base-string list)))

(defparser simple-base-string/terminated (&optional (length 0) (terminator #x00))
  (for ((list (prog1 (rep (satisfies (lambda (byte) (not (= byte terminator)))) length)
                (satisfies (lambda (byte) (= byte terminator))))))
    (bytes-base-string list)))

(defmethod parsonic::expand-expr ((op (eql 'simple-base-string)) &rest args)
  (destructuring-bind (&optional (length '*)) args
    (parsonic::expand
     (case length
       (* `(simple-base-string/terminated))
       (t `(simple-base-string/fixed-length ,length))))))

(defmethod lisp-type-expr ((name (eql 'simple-base-string)) &rest args)
  (declare (ignore args))
  'simple-base-string)
