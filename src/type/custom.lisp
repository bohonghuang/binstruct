(in-package #:binstruct)

(defmethod expand-type-expr ((name (eql 'custom)) &rest args)
  (destructuring-bind (actual-type reader &optional writer) args
    (declare (ignore writer))
    (with-gensyms (value)
      `(for ((,value ,(expand-type actual-type)))
         (funcall ,reader ,value)))))

(defmethod lisp-type-expr ((name (eql 'custom)) &rest args)
  (declare (ignore args))
  t)
