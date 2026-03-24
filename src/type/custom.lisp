(in-package #:binstruct)

(defparser custom (parser &optional (reader #'identity) (writer #'identity))
  (for ((value parser))
    writer
    (funcall reader value)))

(defmethod expand-type-expr ((name (eql 'custom)) &rest args)
  (destructuring-bind (type &optional (reader '#'identity) (writer '#'identity)) args
    (declare (ignore writer))
    (with-gensyms (value)
      `(for ((,value ,(expand-type type)))
         (funcall ,reader ,value)))))

(defmethod lisp-type-expr ((name (eql 'custom)) &rest args)
  (declare (ignore args))
  t)
