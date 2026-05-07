(in-package #:binstruct)

(declaim (inline signed-unsigned-integer))
(defun signed-unsigned-integer (signed nbits)
  (if (minusp signed)
      (+ signed (ash 1 nbits))
      signed))

(declaim (ftype (function (emitter-output non-negative-integer non-negative-fixnum)) emit-unsigned-integer/le))
(defun emit-unsigned-integer/le (output integer n)
  (loop :for shift :of-type non-negative-fixnum :from 0 :below n :by 8
        :do (funcall output (ldb (byte 8 shift) integer))))

(declaim (ftype (function (emitter-output non-negative-integer non-negative-fixnum)) emit-unsigned-integer/be))
(defun emit-unsigned-integer/be (output integer n)
  (loop :for shift :of-type non-negative-fixnum :downfrom (- n 8) :to 0 :by 8
        :do (funcall output (ldb (byte 8 shift) integer))))

(defmethod expand-writer-type-expr ((name (eql 'signed-byte)) &rest args)
  (let ((*value* `(signed-unsigned-integer ,*value* . ,args)))
    (expand-writer-type `(unsigned-byte . ,args))))

(defmethod expand-writer-type-expr ((name (eql 'unsigned-byte)) &rest args)
  (destructuring-bind (n) args
    (ecase *endian*
      (:little `(emit-unsigned-integer/le ,*output* ,*value* ,n))
      (:big `(emit-unsigned-integer/be ,*output* ,*value* ,n)))))
