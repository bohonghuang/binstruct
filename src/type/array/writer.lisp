(in-package #:binstruct)

(defmethod expand-writer-type-expr ((name (eql 'array)) &rest args)
  (destructuring-bind (element-type (length)) args
    (declare (ignore length))
    (with-gensyms (element)
      `(loop :for ,element :across ,*value*
             :do (progn ,(expand-writer-type element-type))))))
