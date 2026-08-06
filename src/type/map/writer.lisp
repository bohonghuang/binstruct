(in-package #:binstruct)

(defmethod expand-writer-type-expr ((name (eql 'values)) &rest args)
  (destructuring-bind (type) args
    (expand-writer-type type)))

(defmethod expand-writer-type-expr ((name (eql 'map)) &rest args)
  (destructuring-bind (type &optional (reader '#'identity) (writer '#'identity)) args
    (declare (ignore reader))
    (let ((*value* `(funcall ,writer ,*value*)))
      (expand-writer-type type))))
