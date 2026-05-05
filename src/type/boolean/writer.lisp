(in-package #:binstruct)

(defmethod expand-writer-type-expr ((name (eql 'boolean)) &rest args)
  (destructuring-bind (&optional (type '(unsigned-byte 8))) args
    (let ((*value* `(if ,*value* 1 0)))
      (expand-writer-type type))))
