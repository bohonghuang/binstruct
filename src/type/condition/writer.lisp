(in-package #:binstruct)

(defmethod expand-write-type-expr ((name (eql 'satisfies)) &rest args)
  (destructuring-bind (type &optional predicate) args
    (declare (ignore predicate))
    (expand-write-type type)))

(defmethod expand-write-type-expr ((name (eql 'ecase)) &rest args)
  (destructuring-bind (object &rest clauses) args
    `(ecase ,object
       . ,(loop :for (key type) :in clauses
                :collect `(,key ,(expand-write-type type))))))

(defmethod expand-write-type-expr ((name (eql 'or)) &rest args)
  `(etypecase ,*value*
     . ,(loop :for arg :in args
              :collect `(,(lisp-type arg) ,(expand-write-type arg)))))
