(in-package #:binstruct)

(defmethod lisp-type-expr ((name (eql 'position)) &rest args)
  (destructuring-bind () args
    'non-negative-fixnum))

(defparser skip (n)
  (let ((position (position)))
    (declare (type non-negative-fixnum position))
    (position (+ position n))))

(defmethod expand-type-expr ((name (eql 'peek)) &rest args)
  (destructuring-bind (type &optional position) args
    (if position
        (with-gensyms (current)
          `(peek
            (let ((,current (position)))
              (skip (the non-negative-fixnum (- ,position ,current)))
              ,(expand-type-unit type))))
        `(peek ,(expand-type type)))))

(defmethod lisp-type-expr ((name (eql 'peek)) &rest args)
  (destructuring-bind (type &optional position) args
    (declare (ignore position))
    (lisp-type type)))

(defmethod expand-type-expr ((name (eql 'pointer)) &rest args)
  (destructuring-bind (data-type pointer-type &optional (base 0)) args
    (let ((slot (car *slots*)))
      (nconcf (cdr *slots*) (list `(,(first slot) ,(second slot) :type (peek ,data-type (+ ,base ,(first slot))))))
      (setf (second slot) 0))
    (expand-type pointer-type)))

(defmethod lisp-type-expr ((name (eql 'pointer)) &rest args)
  (destructuring-bind (data-type &rest args) args
    (declare (ignore args))
    (lisp-type data-type)))
