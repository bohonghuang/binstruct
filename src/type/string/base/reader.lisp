(in-package #:binstruct)

(defparser base-char ()
  (for ((code (unsigned-byte 8)))
    (declare (type (unsigned-byte 8) code))
    (code-char code)))

(declaim (ftype (function (list) (values (simple-array base-char (*)))) bytes-base-string))
(defun bytes-base-string (list)
  (loop :for cons :on list
        :for code :of-type (unsigned-byte 8) := (car cons)
        :for end := (if (zerop code) end cons)
        :do (setf (car cons) (code-char code))
        :finally (return
                   (if end
                       (let ((cdr (shiftf (cdr end) nil)))
                         (prog1 (coerce list '(simple-array base-char (*)))
                           (setf (cdr end) cdr)))
                       #.(coerce "" 'simple-base-string)))))

(defparser simple-base-string/fixed-length (size)
  (for ((list (rep (unsigned-byte 8) size size)))
    (declare (type list list))
    (bytes-base-string list)))

(defparser positive-byte ()
  (satisfies (lambda (byte) (not (eql byte #x00)))))

(defparser simple-base-string/null-terminated ()
  (for ((list (prog1 (rep (positive-byte)) (eql #x00))))
    (declare (type list list))
    (bytes-base-string list)))

(defmethod parsonic::expand-expr ((op (eql 'simple-base-string)) &rest args)
  (destructuring-bind (&optional (length '*)) args
    (parsonic::expand
     (case length
       (* `(simple-base-string/null-terminated))
       (t `(simple-base-string/fixed-length ,length))))))

(defmethod lisp-type-expr ((name (eql 'simple-base-string)) &rest args)
  (declare (ignore args))
  'simple-base-string)
