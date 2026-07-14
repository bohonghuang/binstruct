(in-package #:binstruct)

(defun float->single-float-bits (float)
  (single-float-bits float))

(defun float->double-float-bits (float)
  (double-float-bits float))

(defun float->ieee-float-bits (float e f)
  (cond
    ((and (= e 8) (= f 23))
     (float->single-float-bits float))
    ((and (= e 11) (= f 52))
     (float->double-float-bits float))
    (t
     (multiple-value-bind (mantissa exp sign)
         (integer-decode-float float)
       (let* ((bias (1- (ash 1 (1- e))))
              (sign-bit (if (minusp sign) 1 0)))
         (if (zerop mantissa)
             (ash sign-bit (+ e f))
             (let ((ieee-exp (+ exp (integer-length mantissa) -1 bias)))
               (if (<= ieee-exp 0)
                   ;; denormal: fraction = mantissa << (1 - bias - f - exp)
                   (let ((fraction (ash mantissa (- 1 bias f exp))))
                     (logior (ash sign-bit (+ e f))
                             (ldb (byte f 0) (if (plusp fraction) fraction 0))))
                   ;; normal: fraction = mantissa mod 2^f
                   (let ((fraction (ldb (byte f 0) mantissa)))
                     (logior (ash sign-bit (+ e f))
                             (ash ieee-exp f)
                             fraction))))))))))

(defmethod expand-writer-type-expr ((name (eql 'ieee-float)) &rest args)
  (destructuring-bind (e f) args
    (let ((*value* `(float->ieee-float-bits ,*value* ,e ,f)))
      (expand-writer-type `(unsigned-byte ,(+ 1 e f))))))

(defmethod expand-writer-type-expr ((name (eql 'single-float)) &rest args)
  (declare (ignore args))
  (expand-writer-type '(ieee-float 8 23)))

(defmethod expand-writer-type-expr ((name (eql 'double-float)) &rest args)
  (declare (ignore args))
  (expand-writer-type '(ieee-float 11 52)))
