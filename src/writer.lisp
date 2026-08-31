(in-package #:binstruct)

(defvar *value*)
(defvar *output*)
(defvar *inline*)

(deftype emitter ()
  '(function (emitter-output &rest t)))

(deftype emitter-output ()
  '(function ((or (unsigned-byte 8) (simple-array (unsigned-byte 8) (*))) &optional non-negative-fixnum) (values non-negative-fixnum)))

(declaim (ftype (function (emitter-output (unsigned-byte 8))) emitter-output-byte)
         (inline emitter-output-byte))
(defun emitter-output-byte (output byte)
  (funcall output byte))

(declaim (ftype (function (emitter-output) (values non-negative-fixnum)) emitter-output-position))
(defun emitter-output-position (output)
  (funcall output #.(make-array 0 :element-type '(unsigned-byte 8))))

(declaim (ftype (function (non-negative-fixnum emitter-output) (values non-negative-fixnum)) (setf emitter-output-position)))
(defun (setf emitter-output-position) (position output)
  (funcall output #.(make-array 0 :element-type '(unsigned-byte 8)) position)
  position)

(defun stream-emitter-output (stream)
  (lambda (data &optional position)
    (if position
        (file-position stream position)
        (etypecase data
          ((simple-array (unsigned-byte 8) (*)) (write-sequence data stream) (file-position stream))
          ((unsigned-byte 8) (write-byte data stream))))))

(defun vector-emitter-output (&optional (vector (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
  (assert (adjustable-array-p vector))
  (assert (array-has-fill-pointer-p vector))
  (let ((i (length vector)))
    (declare (type non-negative-fixnum i))
    (values
     (named-lambda output (data &optional position)
       (if position
           (setf i position)
           (etypecase data
             ((simple-array (unsigned-byte 8) (*))
              (loop :for elem :across data
                    :do (output elem)
                    :finally (return i)))
             ((unsigned-byte 8)
              (cond
                ((< i (length vector))
                 (setf (aref vector i) data))
                ((= i (length vector))
                 (vector-push-extend data vector))
                (t (unless (< i (array-total-size vector))
                     (adjust-array vector (1+ i)))
                   (setf (fill-pointer vector) (1+ i)
                         (aref vector i) data)))
              (assert (<= (incf i) (length vector)))))))
     vector)))

(defgeneric ensure-emitter-output (object)
  (:method ((function function))
    function)
  (:method ((stream stream))
    (stream-emitter-output stream))
  (:method ((vector vector))
    (vector-emitter-output vector)))

(define-constant +emitter-name-prefix+ (string '#:emitter/) :test #'string=)

(defun emitter-name-symbol (name &optional (intern t))
  (let ((package (if (eq (symbol-package name) #.(find-package :cl)) #.(find-package '#:binstruct) (symbol-package name)))
        (name (concatenate 'string +emitter-name-prefix+ (symbol-name name))))
    (if intern (intern name package) (find-symbol name package))))

(defgeneric expand-writer-type-expr (name &rest args)
  (:method (name &rest args)
    `(progn
       ,(finish-writer-partial-byte)
       (,(emitter-name-symbol name) ,*output* ,*value* . ,args))))

(defun expand-writer-type (desc)
  (apply #'expand-writer-type-expr (ensure-list desc)))

(defmethod expand-writer-type-expr ((name (eql 'inline)) &rest args)
  (destructuring-bind (type) args
    (push type *inline*)
    `(funcall ,type ,*output* ,*value*)))

(defun expand-writer-type-unit (type &key (endian *endian*) (offset 0) (output *output*) (value *value*) (slots `((,(gensym) type-default-value :type ,type))))
  `(progn
     ,(when (and (boundp '*output*) (boundp '*offset*))
        (finish-writer-partial-byte))
     ,(once-only (output value)
        (let ((*endian* endian)
              (*offset* offset)
              (*output* output)
              (*value* value)
              (*slots* slots))
          `(progn
             ,output ,value
             ,(expand-writer-type type)
             ,(finish-writer-partial-byte))))))
