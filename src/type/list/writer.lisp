(in-package #:binstruct)

(defmethod expand-write-type-expr ((name (eql 'null)) &rest args)
  (destructuring-bind () args))

(defmethod expand-write-type-expr ((name (eql 'cons)) &rest args)
  (destructuring-bind (car cdr) args
    `(progn
       ,(let ((*value* `(car ,*value*)))
          (expand-write-type car))
       ,(let ((*value* `(cdr ,*value*)))
          (expand-write-type cdr)))))

(defmethod expand-write-type-expr ((name (eql 'list)) &rest args)
  (expand-write-type (expand-list-type (cons name args))))
