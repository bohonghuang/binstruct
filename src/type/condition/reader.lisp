(in-package #:binstruct)

(defparser predicate (parser predicate)
  (let ((value parser))
    (rep (or) (if (funcall predicate value) 0 1))
    (constantly value)))

(defmethod parsonic::expand-expr :around ((op (eql 'satisfies)) &rest args)
  (destructuring-bind (predicate-or-parser &optional (predicate nil predicatep)) args
    (if predicatep
        (parsonic::expand `(predicate ,predicate-or-parser ,predicate))
        (call-next-method))))

(defmethod expand-reader-type-expr ((name (eql 'satisfies)) &rest args)
  (destructuring-bind (type
                       &optional
                         (predicate
                          (with-gensyms (value)
                            (assert (equal (getf (first *slots*) :type) (cons name args)))
                            `(lambda (,value) (equalp ,value ,(second (car *slots*)))))
                          predicatep))
      args
    (let ((constant (cond
                      ((not predicatep) (second (car *slots*)))
                      ((typep predicate '(cons (or (eql curry) (eql curry))
                                          (cons (cons (eql function) (cons (or (eql eq) (eql eql) (eql equal) (eql equalp) (eql string=)) null))
                                           (cons t null))))
                       (caddr predicate))
                      (t #1='#:nil))))
      (unless (eq constant #1#)
        (switch (type :test #'equal)
          ('(unsigned-byte 8) (return-from expand-reader-type-expr `(eql ,constant))))))
    (with-gensyms (value)
      `(let ((,value ,(expand-reader-type type)))
         (rep (or) (if (funcall ,predicate ,value) 0 1))
         (constantly ,value)))))

(defmethod lisp-type-expr ((name (eql 'satisfies)) &rest args)
  (destructuring-bind (type &optional predicate) args
    (declare (ignore predicate))
    (lisp-type type)))

(defmethod parsonic::expand-expr ((name (eql 'ecase)) &rest args)
  (destructuring-bind (object &rest clauses) args
    (parsonic::expand
     (with-gensyms (keyform)
       `((lambda (,keyform)
           (ecase ,keyform
             . ,(loop :for (key type) :in clauses
                      :collect `(,key (parser ,type)))))
         (constantly ,object))))))

(defmethod expand-reader-type-expr ((name (eql 'ecase)) &rest args)
  (destructuring-bind (object &rest clauses) args
    `(ecase ,object . ,(loop :for (key type) :in clauses :collect `(,key ,(expand-reader-type-unit type))))))

(defmethod lisp-type-expr ((name (eql 'ecase)) &rest args)
  (destructuring-bind (object &rest clauses) args
    (declare (ignore object))
    `(or . ,(mapcar (compose #'lisp-type #'second) clauses))))

(defmethod expand-reader-type-expr ((name (eql 'or)) &rest args)
  `(or . ,(loop :for type :in args :collect (expand-reader-type-unit type))))

(defmethod lisp-type-expr ((name (eql 'or)) &rest args)
  `(or . ,(loop :for type :in args :collect (lisp-type type))))
