(in-package #:binstruct)

(defmethod expand-write-type-expr ((name (eql 'array)) &rest args)
  (destructuring-bind (element-type (length)) args
    (declare (ignore length))
    (with-gensyms (element)
      `(loop :for ,element :across ,*value*
             :do (progn ,(expand-write-type element-type))))))
