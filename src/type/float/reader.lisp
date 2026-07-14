(in-package #:binstruct)

(defun single-float-bits->float (bits)
  (bits-single-float bits))

(defun double-float-bits->float (bits)
  (bits-double-float bits))

(defun ieee-float-bits->float (bits e f)
  (cond
    ((and (= e 8) (= f 23))
     (single-float-bits->float bits))
    ((and (= e 11) (= f 52))
     (double-float-bits->float bits))
    (t
     (let* ((sign (ldb (byte 1 (+ e f)) bits))
            (exponent (ldb (byte e f) bits))
            (fraction (ldb (byte f 0) bits))
            (bias (1- (ash 1 (1- e))))
            (maxexp (1- (ash 1 e)))
            (float-type (if (<= (+ 1 e f) 32) 'single-float 'double-float))
            (result
              (cond
                ((zerop exponent)
                 (if (zerop fraction)
                     (coerce 0 float-type)
                     (scale-float (coerce fraction float-type) (- 1 bias f))))
                ((= exponent maxexp)
                 (error "Inf/NaN not supported for custom float sizes"))
                (t
                 (scale-float (coerce (+ fraction (ash 1 f)) float-type)
                              (- exponent bias f))))))
       (if (zerop sign) result (- result))))))

(defmethod expand-reader-type-expr ((name (eql 'ieee-float)) &rest args)
  (destructuring-bind (e f) args
    (with-gensyms (bits)
      `(for ((,bits ,(expand-reader-type `(unsigned-byte ,(+ 1 e f)))))
         (ieee-float-bits->float ,bits ,e ,f)))))

(defmethod expand-reader-type-expr ((name (eql 'single-float)) &rest args)
  (declare (ignore args))
  (expand-reader-type '(ieee-float 8 23)))

(defmethod expand-reader-type-expr ((name (eql 'double-float)) &rest args)
  (declare (ignore args))
  (expand-reader-type '(ieee-float 11 52)))

(defmethod lisp-type-expr ((name (eql 'ieee-float)) &rest args)
  (destructuring-bind (e f) args
    (if (<= (+ 1 e f) 32) 'single-float 'double-float)))

(defmethod lisp-type-expr ((name (eql 'single-float)) &rest args)
  (declare (ignore args))
  'single-float)

(defmethod lisp-type-expr ((name (eql 'double-float)) &rest args)
  (declare (ignore args))
  'double-float)

(defmethod parsonic::expand-expr ((name (eql 'ieee-float)) &rest args)
  (parsonic::expand (expand-reader-type-unit (cons name args))))
