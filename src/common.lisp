(in-package #:binstruct)

(defvar *endian*)
(defvar *offset*)
(defvar *slots*)
(defvar *bindings*)
(defvar *place*)
(defvar *positions*)

(defconstant +endian-default+ :little)

(declaim (type list *positions*))

(defgeneric lisp-type-expr (name &rest args)
  (:method (name &rest args)
    (if args (cons name args) name)))

(defun lisp-type (type)
  (apply #'lisp-type-expr (ensure-list type)))

(defun slot-name (&optional (slot (first *slots*)))
  (car slot))

(defun slot-type (&optional (slot (first *slots*)))
  (getf slot :type))

(define-constant +excluded-slot-prefixes+ '(#\$ #\%) :test #'equal)

(defun slot-excluded-p (&optional (slot (first *slots*)))
  (if (slot-name slot) (member (aref (symbol-name (slot-name slot)) 0) +excluded-slot-prefixes+) t))

(defparser inline (parser)
  parser)

(defmethod lisp-type-expr ((name (eql 'inline)) &rest args)
  (declare (ignore args))
  t)

(define-condition partial-byte-error (error)
  ((remainder :initarg :remainder :reader partial-byte-error-remainder))
  (:report (lambda (c s) (format s "The current type requires byte alignment, but ~D bit~:P remain." (partial-byte-error-remainder c)))))
