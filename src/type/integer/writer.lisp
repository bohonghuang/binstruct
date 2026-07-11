(in-package #:binstruct)

(declaim (inline signed-unsigned-integer))
(defun signed-unsigned-integer (signed nbits)
  (if (minusp signed)
      (+ signed (ash 1 nbits))
      signed))

(declaim (ftype (function (emitter-output non-negative-integer non-negative-fixnum)) emit-unsigned-integer/le))
(defun emit-unsigned-integer/le (output integer n)
  (loop :for shift :of-type non-negative-fixnum :from 0 :below n :by 8
        :do (emitter-output-byte output (ldb (byte 8 shift) integer))))

(declaim (ftype (function (emitter-output non-negative-integer non-negative-fixnum)) emit-unsigned-integer/be))
(defun emit-unsigned-integer/be (output integer n)
  (loop :for shift :of-type non-negative-fixnum :downfrom (- n 8) :to 0 :by 8
        :do (emitter-output-byte output (ldb (byte 8 shift) integer))))

(defmethod expand-writer-type-expr ((name (eql 'signed-byte)) &rest args)
  (let ((*value* `(signed-unsigned-integer ,*value* . ,args)))
    (expand-writer-type `(unsigned-byte . ,args))))

(defun unsigned-integer-emitter ()
  (ecase *endian*
    (:little 'emit-unsigned-integer/le)
    (:big 'emit-unsigned-integer/be)))

(declaim (inline make-writer-bitfield))
(defstruct writer-bitfield
  (output nil :type t)
  (value nil :type integer))

(defun finish-writer-partial-byte ()
  (when-let ((offset (shiftf (get *output* 'offset) nil)))
    `(progn
       (,(unsigned-integer-emitter) (writer-bitfield-output ,*output*)
        (writer-bitfield-value ,*output*) ,(* (- (setf *offset* (ceiling *offset*)) offset) 8))
       (setf ,*output* (writer-bitfield-output ,*output*)))))

(defmethod expand-writer-type-expr ((name (eql 'unsigned-byte)) &rest args)
  (destructuring-bind (n &aux (offset *offset*)) args
    (incf *offset* (/ n 8))
    (if (integerp offset)
        (if (integerp *offset*)
            `(,(unsigned-integer-emitter) ,*output* ,*value* ,n)
            (progn
              (assert (null (get *output* 'offset)))
              (setf (get *output* 'offset) offset)
              `(setf ,*output* (make-writer-bitfield :output ,*output* :value ,*value*))))
        `(progn
           (setf (ldb (byte ,n ,(* (- offset (get *output* 'offset)) 8)) (writer-bitfield-value ,*output*)) ,*value*)
           ,(when (integerp *offset*)
              `(progn
                 (,(unsigned-integer-emitter) (writer-bitfield-output ,*output*) (writer-bitfield-value ,*output*) ,(* (- *offset* (shiftf (get *output* 'offset) nil)) 8))
                 (setf ,*output* (writer-bitfield-output ,*output*))))))))
