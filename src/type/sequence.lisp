(in-package #:binstruct)

(defmethod lisp-type-expr ((name (eql 'simple-array)) &rest args)
  (destructuring-bind (type (length)) args
    (declare (ignore length))
    `(simple-array ,(lisp-type type) (*))))

(defparser sequence (element length &optional (type 'simple-array))
  (for ((list (rep element length length)))
    (declare (type list list))
    (coerce list type)))

(defmethod expand-type-expr ((name (eql 'simple-array)) &rest args)
  (destructuring-bind (type (length)) args
    `(sequence ,(expand-type-unit type) ,length ',(lisp-type (cons name args)))))

(defmethod lisp-type-expr ((name (eql 'array)) &rest args)
  (cons name (cdr (apply #'lisp-type-expr 'simple-array args))))

(defmethod expand-type-expr ((name (eql 'array)) &rest args)
  (destructuring-bind (type (length)) args
    `(sequence ,(expand-type-unit type) ,length ',(lisp-type (cons name args)))))
