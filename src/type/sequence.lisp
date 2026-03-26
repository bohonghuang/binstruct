(in-package #:binstruct)

(defparser sequence (element length &optional (type 'simple-array))
  (for ((list (rep element length length)))
    (declare (type list list))
    (unless (eq type 'null)
      (coerce list type))))

(defparser skip (n)
  (let ((position (position)))
    (declare (type non-negative-fixnum position))
    (position (+ position n))))

(defun expand-array-type (name &rest args)
  (finish-partial-byte)
  (destructuring-bind (type (length)) args
    (if (or (car (first *slots*)) (not (equal (lisp-type type) '(unsigned-byte 8))))
        `(sequence ,(expand-type-unit type) ,length ',(lisp-type (cons name args)))
        `(skip ,length))))

(defmethod parsonic::expand-expr ((name (eql 'simple-array)) &rest args)
  (destructuring-bind (type (length)) args
    (parsonic::expand `(sequence ,type ,length ',(lisp-type (cons name args))))))

(defmethod expand-type-expr ((name (eql 'simple-array)) &rest args)
  (apply #'expand-array-type name args))

(defmethod lisp-type-expr ((name (eql 'simple-array)) &rest args)
  (destructuring-bind (type (length)) args
    (declare (ignore length))
    `(simple-array ,(lisp-type type) (*))))

(defmethod parsonic::expand-expr ((name (eql 'array)) &rest args)
  (destructuring-bind (type (length)) args
    (parsonic::expand `(sequence ,type ,length ',(lisp-type (cons name args))))))

(defmethod expand-type-expr ((name (eql 'array)) &rest args)
  (apply #'expand-array-type name args))

(defmethod lisp-type-expr ((name (eql 'array)) &rest args)
  (cons name (cdr (apply #'lisp-type-expr 'simple-array args))))
