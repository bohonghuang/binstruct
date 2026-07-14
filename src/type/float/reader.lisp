(in-package #:binstruct)

(declaim (ftype (function ((unsigned-byte 32) (mod 32) (mod 32)) (values single-float)) ieee754-float-bits->single-float))
(defun ieee754-float-bits->single-float (bits e f)
  (let* ((sign (ldb (byte 1 (+ e f)) bits))
         (exponent (ldb (byte e f) bits))
         (fraction (ldb (byte f 0) bits))
         (bias (1- (ash 1 (1- e))))
         (maxexp (1- (ash 1 e)))
         (result
           (cond
             ((zerop exponent)
              (if (zerop fraction)
                  0.0s0
                  (scale-float (coerce fraction 'single-float) (- 1 bias f))))
             ((= exponent maxexp)
              (if (zerop fraction)
                  (if (zerop sign)
                      float-features:single-float-positive-infinity
                      float-features:single-float-negative-infinity)
                  float-features:single-float-nan))
             (t (scale-float (coerce (+ fraction (ash 1 f)) 'single-float) (- exponent bias f))))))
    (if (zerop sign) result (- result))))

(declaim (ftype (function ((unsigned-byte 64) (mod 64) (mod 64)) (values double-float)) ieee754-float-bits->double-float))
(defun ieee754-float-bits->double-float (bits e f)
  (let* ((sign (ldb (byte 1 (+ e f)) bits))
         (exponent (ldb (byte e f) bits))
         (fraction (ldb (byte f 0) bits))
         (bias (1- (ash 1 (1- e))))
         (maxexp (1- (ash 1 e)))
         (result
           (cond
             ((zerop exponent)
              (if (zerop fraction)
                  0.0d0
                  (scale-float (coerce fraction 'double-float) (- 1 bias f))))
             ((= exponent maxexp)
              (if (zerop fraction)
                  (if (zerop sign)
                      float-features:double-float-positive-infinity
                      float-features:double-float-negative-infinity)
                  float-features:double-float-nan))
             (t (scale-float (coerce (+ fraction (ash 1 f)) 'double-float) (- exponent bias f))))))
    (if (zerop sign) result (- result))))

(declaim (ftype (function ((unsigned-byte 64) (mod 64) (mod 64)) float) ieee754-float-bits->float))
(defun ieee754-float-bits->float (bits e f)
  (cond
    ((and (= e 8) (= f 23)) (float-features:bits-single-float bits))
    ((and (= e 11) (= f 52)) (float-features:bits-double-float bits))
    ((<= (+ 1 e f) 32) (ieee754-float-bits->single-float bits e f))
    (t (ieee754-float-bits->double-float bits e f))))

(defmethod expand-reader-type-expr ((name (eql 'ieee754-float)) &rest args)
  (destructuring-bind (e f) args
    (with-gensyms (bits)
      `(for ((,bits ,(expand-reader-type `(unsigned-byte ,(+ 1 e f)))))
         (ieee754-float-bits->float ,bits ,e ,f)))))

(defmethod expand-reader-type-expr ((name (eql 'single-float)) &rest args)
  (declare (ignore args))
  (expand-reader-type '(ieee754-float 8 23)))

(defmethod expand-reader-type-expr ((name (eql 'double-float)) &rest args)
  (declare (ignore args))
  (expand-reader-type '(ieee754-float 11 52)))

(defmethod lisp-type-expr ((name (eql 'ieee754-float)) &rest args)
  (destructuring-bind (e f) args
    (if (<= (+ 1 e f) 32) 'single-float 'double-float)))

(defmethod parsonic::expand-expr ((name (eql 'ieee754-float)) &rest args)
  (parsonic::expand (expand-reader-type-unit (cons name args))))
