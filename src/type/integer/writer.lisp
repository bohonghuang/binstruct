(in-package #:binstruct)

(declaim (inline signed-unsigned-integer))
(defun signed-unsigned-integer (signed nbits)
  (if (minusp signed)
      (+ signed (ash 1 nbits))
      signed))

(declaim (ftype (function (writer-output non-negative-integer non-negative-fixnum)) write-unsigned-integer/le))
(defun write-unsigned-integer/le (output integer n)
  (loop :for shift :of-type non-negative-fixnum :from 0 :below n :by 8
        :do (funcall output (ldb (byte 8 shift) integer))))

(declaim (ftype (function (writer-output non-negative-integer non-negative-fixnum)) write-unsigned-integer/be))
(defun write-unsigned-integer/be (output integer n)
  (loop :for shift :of-type non-negative-fixnum :downfrom (- n 8) :to 0 :by 8
        :do (funcall output (ldb (byte 8 shift) integer))))

(defmethod expand-write-type-expr ((name (eql 'signed-byte)) &rest args)
  (let ((*value* `(signed-unsigned-integer ,*value*)))
    (expand-write-type `(unsigned-byte . ,args))))

(defmethod expand-write-type-expr ((name (eql 'unsigned-byte)) &rest args)
  (destructuring-bind (n) args
    (ecase *endian*
      (:little `(write-unsigned-integer/le ,*output* ,*value* ,n))
      (:big `(write-unsigned-integer/be ,*output* ,*value* ,n)))))
