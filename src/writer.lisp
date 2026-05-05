(in-package #:binstruct)

(defvar *value*)
(defvar *output*)

(deftype writer ()
  '(function (writer-output &rest t)))

(deftype writer-output ()
  '(function ((or (unsigned-byte 8) negative-fixnum))))

(define-constant +writer-name-prefix+ (string '#:writer/) :test #'string=)

(defun writer-name-symbol (name &optional (intern t))
  (let ((package (if (eq (symbol-package name) #.(find-package :cl)) #.(find-package '#:binstruct) (symbol-package name)))
        (name (concatenate 'string +writer-name-prefix+ (symbol-name name))))
    (if intern (intern name package) (find-symbol name package))))

(defgeneric expand-write-type-expr (name &rest args))

(defun expand-write-type (desc)
  (apply #'expand-write-type-expr (ensure-list desc)))
