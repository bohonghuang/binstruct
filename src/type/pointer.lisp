(in-package #:binstruct)

(defmethod lisp-type-expr ((name (eql 'position)) &rest args)
  (destructuring-bind () args
    'non-negative-fixnum))

(defgeneric lisp-type-expr-default-value (name &rest args)
  (:method (name &rest args)
    (declare (ignore args))
    (let ((class (find-class name)))
      (check-type class class)
      (allocate-instance class)))
  (:method ((name (eql 't)) &rest args)
    (declare (ignore args))
    nil)
  (:method ((name (eql 'boolean)) &rest args)
    (declare (ignore args))
    nil)
  (:method ((name (eql 'unsigned-byte)) &rest args)
    (declare (ignore args))
    0)
  (:method ((name (eql 'signed-byte)) &rest args)
    (declare (ignore args))
    0)
  (:method ((name (eql 'cons)) &rest args)
    (destructuring-bind (car cdr) args
      (cons (lisp-type-default-value car) (lisp-type-default-value cdr))))
  (:method ((name (eql 'simple-base-string)) &rest args)
    (destructuring-bind (length) args
      (make-array (or length 0) :element-type 'base-char)))
  (:method ((name (eql 'simple-array)) &rest args)
    (destructuring-bind (element-type (length)) args
      (make-array (case length (* 0) (t length)) :element-type element-type)))
  (:method ((name (eql 'array)) &rest args)
    (apply #'lisp-type-expr-default-value 'simple-array args)))

(defun lisp-type-default-value (type)
  (apply #'lisp-type-expr-default-value (ensure-list type)))

(defun type-default-value (type)
  (lisp-type-default-value (lisp-type type)))

(defmethod expand-type-expr ((name (eql 'peek)) &rest args)
  (destructuring-bind (type &optional position) args
    (if position
        `(peek (progn (position ,position) ,(expand-type-unit type)))
        `(peek ,(expand-type type)))))

(defmethod lisp-type-expr ((name (eql 'peek)) &rest args)
  (destructuring-bind (type &optional position) args
    (declare (ignore position))
    (lisp-type type)))

(defmethod expand-type-expr ((name (eql 'pointer)) &rest args)
  (destructuring-bind (data-type pointer-type &optional (base 0)) args
    (let ((slot (first *slots*)))
      (nconcf (cdr *slots*) (list `(,(first slot) ,(second slot) :type (pointer-1 ,data-type ,base))))
      (setf (second slot) 0))
    (expand-type pointer-type)))

(defmethod lisp-type-expr ((name (eql 'pointer)) &rest args)
  (destructuring-bind (data-type &rest args) args
    (declare (ignore args))
    (lisp-type data-type)))

(defun pointer-base (name)
  (declare (ignore name))
  nil)

(defun (setf pointer-base) (value name)
  (declare (ignore name))
  value)

(defmethod expand-type-expr ((name (eql 'pointer-1)) &rest args)
  (destructuring-bind (data-type base &aux (slot (first *slots*))) args
    (if (or (constantp base) (not (symbolp base)) (member base *bindings* :key #'car))
        (expand-type `(peek ,data-type (+ ,base ,(car slot))))
        (with-gensyms (offset)
          (setf (car (find (car slot) *bindings* :from-end t :key #'car)) offset)
          (nconcf (cdr *slots*) (list `(nil nil :type (pointer-2 ,data-type ,base ,offset ,(first slot)))))
          `(constantly ,(type-default-value (lisp-type (cons name args))))))))

(defmethod lisp-type-expr ((name (eql 'pointer-1)) &rest args)
  (destructuring-bind (data-type &rest args) args
    (declare (ignore args))
    (lisp-type data-type)))

(defmethod expand-type-expr ((name (eql 'pointer-2)) &rest args)
  (destructuring-bind (data-type base offset self) args
    (with-gensyms (var)
      `((lambda (,var ,offset)
          (if ,var
              (parser (peek (for ((nil (position (+ ,var ,offset)))
                                  (,var ,(expand-type-unit data-type)))
                              (setf ,self ,var))))
              (parser (constantly (assert nil)))))
        (constantly (pointer-base ',base))
        (constantly ,offset)))))

(defmethod lisp-type-expr ((name (eql 'pointer-2)) &rest args)
  (destructuring-bind (data-type &rest args) args
    (declare (ignore args))
    (lisp-type data-type)))
