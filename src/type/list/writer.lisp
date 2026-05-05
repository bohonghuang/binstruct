(in-package #:binstruct)

(defmethod expand-writer-type-expr ((name (eql 'null)) &rest args)
  (destructuring-bind () args))

(defmethod expand-writer-type-expr ((name (eql 'cons)) &rest args)
  (destructuring-bind (car cdr) args
    `(progn
       ,(let ((*value* `(car ,*value*)))
          (expand-writer-type car))
       ,(let ((*value* `(cdr ,*value*)))
          (expand-writer-type cdr)))))

(defmethod expand-writer-type-expr ((name (eql 'list)) &rest args)
  (expand-writer-type (expand-list-type (cons name args))))
