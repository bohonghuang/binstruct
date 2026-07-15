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
  (destructuring-bind (type &optional (reader '#'identity) (writer '#'identity)) args
    (declare (ignore type writer))
    (when (consp reader)
      (destructuring-case reader
        ((the type function)
         (declare (ignore function))
         (when (consp type)
           (destructuring-case type
             ((function (&rest args) &optional (type 'null))
              (declare (ignore args))
              (if (consp type)
                  (destructuring-case type
                    ((values &optional (type 'null) &rest args)
                     (declare (ignore args))
                     (return-from lisp-type-expr type))
                    ((t &rest args)
                     (declare (ignore args))
                     (return-from lisp-type-expr type)))
                  (return-from lisp-type-expr type))))))))
    t))
