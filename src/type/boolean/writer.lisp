(in-package #:binstruct)

(defmethod expand-writer-type-expr ((name (eql 'boolean)) &rest args)
  (destructuring-bind (&optional (type '(unsigned-byte 8))) args
    (let ((*value* `(plusp ,*value*)))
      (expand-writer-type type))))
