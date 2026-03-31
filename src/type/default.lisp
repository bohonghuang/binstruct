(in-package #:binstruct)

(defun lisp-type-default-value (type)
  (destructuring-case (ensure-list type)
    ((or &rest args)
     (lisp-type-default-value (first args)))
    ((t &rest args)
     (eswitch (type :test #'subtypep)
       ('integer 0)
       ('single-float 0s0)
       ('double-float 0d0)
       ('boolean nil)
       ('cons
        (destructuring-bind (car cdr) args
          `(load-time-value (cons ,(lisp-type-default-value car) ,(lisp-type-default-value cdr)))))
       ('base-string
        (destructuring-bind (&optional (length '*)) args
          (make-array (case length (* 0) (t length)) :element-type 'base-char)))
       ('string "")
       ('vector
        (destructuring-bind (element-type (length)) args
          (make-array (case length (* 0) (t length)) :element-type element-type)))
       ('structure-object
        (let ((constructor (find-symbol (format nil "~A-~A" '#:make type) (symbol-package type))))
          (assert (fboundp constructor))
          `(load-time-value (,constructor))))
       ('t nil)))))

(defun type-default-value (type)
  (lisp-type-default-value (lisp-type type)))
