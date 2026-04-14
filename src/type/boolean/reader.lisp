(in-package #:binstruct)

(defparser boolean (&optional (parser (unsigned-byte 8)))
  (for ((value parser))
    (plusp value)))

(defmethod expand-reader-type-expr ((name (eql 'boolean)) &rest args)
  (destructuring-bind (&optional (type '(unsigned-byte 8))) args
    (with-gensyms (value)
      `(for ((,value ,(expand-reader-type type)))
         (declare (type ,type ,value))
         (plusp ,value)))))

(defmethod lisp-type-expr ((name (eql 'boolean)) &rest args)
  (destructuring-bind (&optional type) args
    (declare (ignore type))
    'boolean))
