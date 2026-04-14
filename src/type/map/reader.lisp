(in-package #:binstruct)

(defparser map (parser &optional (reader #'identity) (writer #'identity))
  (for ((value parser))
    writer
    (funcall reader value)))

(defmethod expand-reader-type-expr ((name (eql 'map)) &rest args)
  (destructuring-bind (type &optional (reader '#'identity) (writer '#'identity)) args
    (declare (ignore writer))
    (with-gensyms (value)
      `(for ((,value ,(expand-reader-type type)))
         (funcall ,reader ,value)))))

(defmethod lisp-type-expr ((name (eql 'map)) &rest args)
  (declare (ignore args))
  t)
