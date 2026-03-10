(in-package #:binstruct)

(defmethod lisp-type-expr ((name (eql 'simple-array)) &rest args)
  (destructuring-bind (type (length)) args
    (declare (ignore length))
    `(simple-array ,(lisp-type type) (*))))

(defmethod expand-type-expr ((name (eql 'simple-array)) &rest args)
  (destructuring-bind (type (length)) args
    (with-gensyms (count list)
      `(let ((,count (constantly ,length)))
         (for ((,list (rep ,(expand-type-unit type) ,count ,count)))
           (declare (type list ,list))
           (coerce ,list ',(lisp-type (cons name args))))))))
