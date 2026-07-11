(in-package #:binstruct)

(defmethod expand-writer-type-expr ((name (eql 'array)) &rest args)
  (destructuring-bind (element-type (length)) args
    (declare (ignore length))
    (with-gensyms (element)
      `(loop :for ,element :across ,*value*
             :do (progn
                   ,(let ((*value* element))
                      (expand-writer-type element-type)))))))

(defmethod expand-writer-type-expr ((name (eql 'simple-array)) &rest args)
  (apply #'expand-writer-type-expr 'array args))
