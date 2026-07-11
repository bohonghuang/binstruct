(in-package #:binstruct)

(defmethod expand-writer-type-expr ((name (eql 'position)) &rest args)
  (declare (ignore args)))
