(in-package #:binstruct)

(defmethod expand-writer-type-expr ((name (eql 'satisfies)) &rest args)
  (destructuring-bind (type &optional predicate) args
    (declare (ignore predicate))
    (expand-writer-type type)))

(defmethod expand-writer-type-expr ((name (eql 'ecase)) &rest args)
  (destructuring-bind (object &rest clauses) args
    `(ecase ,object
       . ,(loop :for (key type) :in clauses
                :collect `(,key ,(expand-writer-type type))))))

(defmethod expand-writer-type-expr ((name (eql 'or)) &rest args)
  `(etypecase ,*value*
     . ,(delete-duplicates
         (loop :for arg :in args
               :collect `(,(lisp-type arg) ,(expand-writer-type arg)))
         :key #'first :test #'type= :from-end t)))
