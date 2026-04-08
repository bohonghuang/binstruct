(in-package #:binstruct)

(defun pointer-base (name)
  (or (assoc name *positions*) (car (push (cons name nil) *positions*))))

(defun pointer-base-position (name &optional callback)
  (let* ((base (pointer-base name))
         (position (cdr base)))
    (etypecase position
      (non-negative-fixnum position)
      (list (when callback (setf (cdr base) (cons callback position))) nil))))

(defun global-position-p (name)
  (and (symbolp name) (eql (position #\$ (symbol-name name)) 0)))

(defmethod expand-type-expr ((name (eql 'peek)) &rest args)
  (destructuring-bind (type &optional position) args
    (if position
        `(peek (progn (position ,position) ,(expand-type-unit type)))
        `(peek ,(expand-type type)))))

(defmethod lisp-type-expr ((name (eql 'peek)) &rest args)
  (destructuring-bind (type &optional position) args
    (declare (ignore position))
    (lisp-type type)))

(defmethod expand-type-expr ((name (eql 'position)) &rest args)
  (destructuring-bind (&aux (name (car (first *slots*)))) args
    (if (global-position-p name)
        (with-gensyms (position pending)
          `(peek
            (let* ((,position (position))
                   (,pending (constantly (shiftf (cdr (pointer-base ',name)) ,position))))
              (rep ((lambda ()
                      (if (consp ,pending)
                          (funcall (the function (pop ,pending)) ,position)
                          (parser (or))))))
              (constantly ,position))))
        '(position))))

(defmethod lisp-type-expr ((name (eql 'position)) &rest args)
  (destructuring-bind () args
    'input-position))

(defmethod expand-type-expr ((name (eql 'pointer)) &rest args)
  (destructuring-bind (data-type pointer-type &optional (base 0)) args
    (let ((slot (first *slots*)))
      (unless (car slot) (setf (car slot) (with-gensyms (pointer) pointer)))
      (nconcf (cdr *slots*) (list `(,(first slot) ,(second slot) :type (pointer-1 ,data-type ,base))))
      (setf (second slot) 0))
    (let ((*place* (constantly '(assert nil))) (*slots* nil))
      (expand-type pointer-type))))

(defmethod lisp-type-expr ((name (eql 'pointer)) &rest args)
  (destructuring-bind (data-type &rest args) args
    (declare (ignore args))
    (lisp-type data-type)))

(defmethod parsonic::expand-expr ((name (eql 'pointer)) &rest args)
  (parsonic::expand (expand-type-unit (cons name args))))

(defmethod expand-type-expr ((name (eql 'pointer-1)) &rest args)
  (destructuring-bind (data-type base &aux (slot (first *slots*))) args
    (with-gensyms (offset)
      (setf (car (find (car slot) *bindings* :from-end t :key #'car)) offset)
      (prog1 (if (global-position-p base)
                 (with-gensyms (result thunk position)
                   `((lambda (,offset)
                       (funcall
                        (lambda (,thunk)
                          (if-let ((,position (pointer-base-position ',base ,thunk)))
                            (funcall ,thunk ,position)
                            (parser (constantly ,(type-default-value data-type)))))
                        (lambda (,position)
                          (declare (type non-negative-fixnum ,position))
                          (parser
                           (cut
                            (let ((,position (constantly (progn ,position))))
                              ((lambda (,result)
                                 ,(funcall *place* result)
                                 (parser (constantly ,result)))
                               ,(expand-type-unit `(peek ,data-type (+ ,position ,offset))))))))))
                     (constantly ,offset)))
                 (expand-type `(peek ,data-type (+ ,base ,offset))))
        (unless (symbol-package (car slot))
          (nconcf (cdr *slots*) (list `(nil nil :type (inline (constantly ,(car slot)))))))))))

(defmethod lisp-type-expr ((name (eql 'pointer-1)) &rest args)
  (lisp-type (first args)))
