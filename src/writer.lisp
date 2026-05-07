(in-package #:binstruct)

(defvar *value*)
(defvar *output*)

(deftype emitter ()
  '(function (emitter-output &rest t)))

(deftype emitter-position ()
  `(integer ,most-negative-fixnum -1))

(declaim (ftype (function (non-negative-fixnum) (values emitter-position)) file-position-emitter-position)
         (inline file-position-emitter-position))
(defun file-position-emitter-position (position)
  (+ most-negative-fixnum position))

(declaim (ftype (function (emitter-position) (values non-negative-fixnum)) emitter-position-file-position)
         (inline emitter-position-file-position))
(defun emitter-position-file-position (position)
  (- position most-negative-fixnum))

(deftype emitter-output ()
  '(function ((or (unsigned-byte 8) (simple-array (unsigned-byte 8) (*)) negative-fixnum)) (values non-negative-fixnum)))

(declaim (ftype (function (emitter-output) (values non-negative-fixnum)) emitter-output-position))
(defun emitter-output-position (output)
  (funcall output #.(make-array 0 :element-type '(unsigned-byte 8))))

(declaim (ftype (function (non-negative-fixnum emitter-output) (values non-negative-fixnum)) (setf emitter-output-position)))
(defun (setf emitter-output-position) (target output)
  (let ((current (emitter-output-position output)))
    (cond
      ((> current target)
       (funcall output (- target current)))
      ((< current target)
       (funcall output (- current (- target current)))))
    target))

(define-constant +emitter-name-prefix+ (string '#:emitter/) :test #'string=)

(defun emitter-name-symbol (name &optional (intern t))
  (let ((package (if (eq (symbol-package name) #.(find-package :cl)) #.(find-package '#:binstruct) (symbol-package name)))
        (name (concatenate 'string +emitter-name-prefix+ (symbol-name name))))
    (if intern (intern name package) (find-symbol name package))))

(defgeneric expand-writer-type-expr (name &rest args)
  (:method (name &rest args)
    (list* (emitter-name-symbol name) *output* *value* args)))

(defun expand-writer-type (desc)
  (apply #'expand-writer-type-expr (ensure-list desc)))
