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
  (loop :for shift :of-type #+sbcl fixnum #-sbcl non-negative-fixnum :downfrom (- n 8) :to 0 :by 8
        :do (emitter-output-byte output (ldb (byte 8 shift) integer))))

(defmethod expand-writer-type-expr ((name (eql 'signed-byte)) &rest args)
  (let ((*value* `(signed-unsigned-integer ,*value* . ,args)))
    (expand-writer-type `(unsigned-byte . ,args))))

(defun unsigned-integer-emitter ()
  (ecase *endian*
    (:little 'emit-unsigned-integer/le)
    (:big 'emit-unsigned-integer/be)))

(declaim (inline make-emitter-bitfield-output))
(defstruct emitter-bitfield-output
  (output nil :type t)
  (value nil :type integer)
  (size 0 :type non-negative-fixnum))

(defmethod ensure-emitter-output ((output emitter-bitfield-output))
  (emitter-bitfield-output-output output))

(defun finish-writer-partial-byte ()
  (when-let ((offset (shiftf (get *output* 'offset) nil)))
    (restart-case (error 'partial-byte-error :remainder (* (- (ceiling offset) offset) 8))
      (pad ()
        :report "Pad remaining bits and continue."
        `(progn
           (,(unsigned-integer-emitter) (emitter-bitfield-output-output ,*output*)
            (emitter-bitfield-output-value ,*output*) ,(* (- (setf *offset* (ceiling *offset*)) offset) 8))
           (setf ,*output* (emitter-bitfield-output-output ,*output*))))
      (ignore ()
        :report "Ignore the partial-byte boundary and continue."
        (setf (get *output* 'offset) offset)))))

(defmethod expand-writer-type-expr ((name (eql 'unsigned-byte)) &rest args)
  (destructuring-bind (n) args
    (let* ((offset *offset*)
           (start (or (get *output* 'offset) offset)))
      (incf *offset* (/ n 8))
      (if (and (integerp offset) (integerp start))
          (if (integerp *offset*)
              `(,(unsigned-integer-emitter) ,*output* ,*value* ,n)
              (progn
                (assert (null (get *output* 'offset)))
                (setf (get *output* 'offset) offset)
                `(setf ,*output* (make-emitter-bitfield-output :output ,*output* :value ,*value*))))
          `(progn
             (setf (ldb (byte ,n ,(* (- offset start) 8)) (emitter-bitfield-output-value ,*output*)) ,*value*)
             ,(when (integerp *offset*)
                `(progn
                   (,(unsigned-integer-emitter) (emitter-bitfield-output-output ,*output*)
                    (emitter-bitfield-output-value ,*output*) ,(let ((bytes (- *offset* (shiftf (get *output* 'offset) nil))))
                                                         (if (integerp bytes)
                                                             `(setf (emitter-bitfield-output-size ,*output*) ,(* bytes 8))
                                                             `(emitter-bitfield-output-size ,*output*))))
                   (setf ,*output* (emitter-bitfield-output-output ,*output*)))))))))

(defmethod expand-writer-type-expr ((name (eql 'bit)) &rest args)
  (declare (ignore args))
  (expand-writer-type '(unsigned-byte 1)))
