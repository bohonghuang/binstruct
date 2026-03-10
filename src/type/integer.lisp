(in-package #:binstruct)

(defun unsigned-byte-8-p (object)
  (typecase object ((unsigned-byte 8) t)))

(defparser unsigned-byte-8 ()
  (satisfies #'unsigned-byte-8-p))

(declaim (ftype (function (list) (values integer)) bytes-unsigned-integer/le))
(defun bytes-unsigned-integer/le (bytes)
  (loop :for byte :in bytes
        :for shift :from 0 :by 8
        :sum (ash byte shift)))

(declaim (ftype (function (list) (values integer)) bytes-unsigned-integer/be))
(defun bytes-unsigned-integer/be (bytes)
  (loop :for byte :in bytes
        :for shift :downfrom (* 8 (1- (length bytes))) :by 8
        :sum (ash byte shift)))

(declaim (inline unsigned-signed-integer))
(defun unsigned-signed-integer (unsigned nbits)
  (if (logbitp (1- nbits) unsigned)
      (- unsigned (ash 1 nbits))
      unsigned))

(defmethod expand-type-expr ((name (eql 'signed-byte)) &rest args)
  (destructuring-bind (n) args
    (with-gensyms (unsigned)
      `(for ((,unsigned ,(expand-type `(unsigned-byte ,n))))
         (the (signed-byte ,n) (unsigned-signed-integer ,unsigned ,n))))))

(defmethod expand-type-expr ((name (eql 'unsigned-byte)) &rest args)
  (destructuring-bind (n) args
    (prog1 (if (and (integerp *offset*) (zerop (mod n 8)))
               (with-gensyms (bytes)
                 `(for ((,bytes (list . ,(loop :repeat (floor n 8) :collect `(unsigned-byte-8)))))
                    (the (unsigned-byte ,n) ,(ecase *endian*
                                               (:big `(bytes-unsigned-integer/be ,bytes))
                                               (:little `(bytes-unsigned-integer/le ,bytes))))))
               (let ((bit-offset (/ *offset* 1/8)))
                 `(constantly (the (unsigned-byte ,n) (ldb (byte ,n ,bit-offset) byte)))))
      (incf *offset* (/ n 8)))))

(defmethod expand-type-expr ((name (eql 'boolean)) &rest args)
  (destructuring-bind (&optional (actual-type '(unsigned-byte 8))) args
    (with-gensyms (actual-value)
      `(for ((,actual-value ,(expand-type actual-type)))
         (declare (type ,actual-type ,actual-value))
         (plusp ,actual-value)))))

(defmethod lisp-type-expr ((name (eql 'boolean)) &rest args)
  (destructuring-bind (&optional actual-type) args
    (declare (ignore actual-type))
    'boolean))
