(in-package #:binstruct)

(declaim (ftype (function (list) (values (simple-array base-char (*)))) bytes-base-string))
(defun bytes-base-string (list)
  (loop :for cons :on list :do (setf (car cons) (code-char (the (unsigned-byte 8) (car cons)))))
  (coerce list '(simple-array base-char (*))))

(defparser terminated-base-string (&optional (length 0) (terminator #x00))
  (for ((list (prog1 (rep (satisfies (lambda (byte) (not (= byte terminator)))) length)
                (satisfies (lambda (byte) (= byte terminator))))))
    (bytes-base-string list)))

(deftype terminated-base-string (&rest args)
  (declare (ignore args))
  '(simple-array base-char (*)))

(defparser simple-base-string (size)
  (for ((list (rep (unsigned-byte-8) size size)))
    (bytes-base-string list)))

(defmethod lisp-type-expr ((name (eql 'simple-base-string)) &rest args)
  (destructuring-bind (length) args
    `(simple-base-string ,(if (and (constantp length) (integerp (eval length))) length '*))))
