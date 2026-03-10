(in-package #:binstruct)

(defmethod expand-type-expr ((name (eql 'magic)) &rest args)
  (destructuring-bind (actual-type
                       &optional
                         (value
                          (progn
                            (assert (equal (getf (car *slots*) :type) (cons name args)))
                            (second (car *slots*)))))
      args
    (with-gensyms (actual-value)
      `(for ((,actual-value ,(expand-type actual-type)))
         (assert (equalp ,actual-value ,value))
         ,actual-value))))

(defmethod lisp-type-expr ((name (eql 'magic)) &rest args)
  (destructuring-bind (actual-type &optional value) args
    (declare (ignore value))
    (lisp-type actual-type)))

(defmethod expand-type-expr ((name (eql 'ecase)) &rest args)
  (destructuring-bind (object &rest clauses) args
    `(funcall
      (lambda ()
        (ecase ,object
          . ,(loop :for (key type) :in clauses
                   :collect `(,key (parser ,(expand-type-unit type)))))))))

(defmethod lisp-type-expr ((name (eql 'ecase)) &rest args)
  (destructuring-bind (object &rest clauses) args
    (declare (ignore object))
    `(or . ,(mapcar (compose #'lisp-type #'second) clauses))))
