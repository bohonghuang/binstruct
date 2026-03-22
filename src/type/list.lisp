(in-package #:binstruct)

(defmethod expand-type-expr ((name (eql 'null)) &rest args)
  (assert (null args))
  '(constantly nil))

(defmethod lisp-type-expr ((name (eql 'cons)) &rest args)
  (destructuring-bind (car cdr) args
    `(cons ,(lisp-type car) ,(lisp-type cdr))))

(defmethod expand-type-expr ((name (eql 'cons)) &rest args)
  (destructuring-bind (car cdr) args
    (with-gensyms (var-car var-cdr)
      `(let* ,(slots-parser-bindings `((,var-car nil :type ,car) (,var-cdr nil :type ,cdr)))
         (constantly (cons ,var-car ,var-cdr))))))

(defun expand-list-type (form)
  (destructuring-ecase form
    ((list &rest args)
     (loop :for form := 'null :then `(cons ,arg ,form)
           :for arg :in (reverse args)
           :finally (return form)))))

(defmethod lisp-type-expr ((name (eql 'list)) &rest args)
  (lisp-type (expand-list-type (cons name args))))

(defmethod expand-type-expr ((name (eql 'list)) &rest args)
  (expand-type (expand-list-type (cons name args))))
