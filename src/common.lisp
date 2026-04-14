(in-package #:binstruct)

(defvar *endian*)
(defvar *offset*)
(defvar *slots*)
(defvar *bindings*)
(defvar *place*)
(defvar *positions*)

(defgeneric lisp-type-expr (name &rest args)
  (:method (name &rest args)
    (if args (cons name args) name)))

(defun lisp-type (type)
  (apply #'lisp-type-expr (ensure-list type)))

(deftype offset ()
  'non-negative-fixnum)

(defun slot-name (&optional (slot (first *slots*)))
  (car slot))

(defun slot-excluded-p (&optional (slot (first *slots*)))
  (or (eq (lisp-type (getf slot :type)) 'offset)
      (null (slot-name slot))))

(defparser inline (parser)
  parser)

(defmethod lisp-type-expr ((name (eql 'inline)) &rest args)
  (declare (ignore args))
  t)
