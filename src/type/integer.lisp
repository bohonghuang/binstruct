(in-package #:binstruct)

(defun unsigned-byte-8-p (object)
  (typecase object ((unsigned-byte 8) t)))

(defparser unsigned-byte-8 ()
  (satisfies #'unsigned-byte-8-p))

(declaim (ftype (function (list) (values integer)) bytes-unsigned-integer/le))
(defun bytes-unsigned-integer/le (bytes)
  (loop :for byte :of-type (unsigned-byte 8) :in bytes
        :for shift :of-type non-negative-fixnum :from 0 :by 8
        :sum (ash byte shift)))

(declaim (ftype (function (list) (values integer)) bytes-unsigned-integer/be))
(defun bytes-unsigned-integer/be (bytes)
  (loop :for byte :of-type (unsigned-byte 8) :in bytes
        :for shift :of-type non-negative-fixnum :downfrom (* 8 (1- (length bytes))) :by 8
        :sum (ash byte shift)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +word-size+ (* (ceiling (log most-positive-fixnum 2) 8) 8))
  (defun unsigned-byte-parser (n endian)
    (with-gensyms (bytes)
      (if (<= n +word-size+)
          `(for ((,bytes (list . ,(loop :repeat (floor n 8) :collect `(unsigned-byte-8)))))
             ,(with-gensyms (byte shift)
                (ecase endian
                  (:big `(loop :for ,byte :of-type (unsigned-byte 8) :in ,bytes
                               :for ,shift :of-type non-negative-fixnum :downfrom ,(- n 8) :by 8
                               :sum (the (unsigned-byte ,n) (ash ,byte ,shift)) :of-type (unsigned-byte ,n)))
                  (:little `(loop :for ,byte :of-type (unsigned-byte 8) :in ,bytes
                                  :for ,shift :of-type non-negative-fixnum :from 0 :by 8
                                  :sum (the (unsigned-byte ,n) (ash ,byte ,shift)) :of-type (unsigned-byte ,n))))))
          `(for ((,bytes (rep (unsigned-byte-8) ,(floor n 8) ,(floor n 8))))
             ,(ecase endian
                (:big `(bytes-unsigned-integer/be ,bytes))
                (:little `(bytes-unsigned-integer/le ,bytes))))))))

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

(defmacro define-unsigned-integer-parsers ()
  (loop :with mappings
        :for n :from 8 :to (max +word-size+ 64) :by 8
        :nconc (loop :for endian :in '(:little :big)
                     :for name := (setf (assoc-value mappings (list `(unsigned-byte ,n) endian) :test #'equal)
                                        (intern (format nil "~A-~D~@[/~A~]" 'unsigned-byte n (when (> n 8) (ecase endian (:little 'le) (:big 'be))))))
                     :when (> n 8) :collect `(defparser ,name () ,(unsigned-byte-parser n endian)))
          :into parsers
        :finally
           (return
             (with-gensyms (name type endian)
               `(progn
                  (defun integer-type-parser (,type &optional (,endian :little))
                    (if-let ((,name (assoc-value ',mappings (list ,type ,endian) :test #'equal)))
                      (list ,name)
                      (progn
                        (assert (eq (first ,type) 'unsigned-byte))
                        (unsigned-byte-parser (second ,type) ,endian))))
                  ,@parsers)))))

(define-unsigned-integer-parsers)

(defmethod expand-type-expr ((name (eql 'unsigned-byte)) &rest args)
  (destructuring-bind (n) args
    (let* ((offset (prog1 *offset* (incf *offset* (/ n 8))))
           (binding (if (integerp offset)
                        (if (zerop (mod n 8))
                            (return-from expand-type-expr (integer-type-parser (cons name args) *endian*))
                            (with-gensyms (byte)
                              (let ((binding `(,byte ,offset)))
                                (setf (get byte 'offset) (cdr binding))
                                (nconcf *bindings* (list binding))
                                binding)))
                        (find-if (rcurry #'get 'offset) *bindings* :from-end t :key #'car))))
      (destructuring-bind (byte value &aux (parser (get byte 'offset)) (start (car parser))) binding
        (declare (ignore value))
        (assert (null (symbol-package byte)))
        (prog1 (let ((bit-offset (/ (- offset start) 1/8)))
                 `(constantly (the (unsigned-byte ,n) (ldb (byte ,n ,bit-offset) ,byte))))
          (when (integerp *offset*)
            (setf (symbol-plist byte) nil
                  (second binding) `(constantly 0)
                  (car parser) (let ((bytes (- *offset* start)) (*offset* 0))
                                 (check-type bytes positive-fixnum)
                                 (expand-type `(unsigned-byte ,(* bytes 8)))))))))))

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
