(in-package #:binstruct)

(defparser unsigned-byte-8 ()
  (satisfies (constantly t)))

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
    (if (<= n +word-size+)
        (let* ((count (floor n 8))
               (bytes (loop :repeat count :collect (with-gensyms (byte) byte)))
               (shifts (ecase endian
                         (:big (loop :for i :from 0 :below count :collect (- n (* 8 (1+ i)))))
                         (:little (loop :for i :from 0 :below count :collect (* 8 i))))))
          `(for ,(loop :for byte :in bytes :collect `(,byte (unsigned-byte-8)))
             (declare (type (unsigned-byte 8) . ,bytes))
             (the (unsigned-byte ,n) (logior . ,(loop :for byte :in bytes :for shift :in shifts :collect `(ash ,byte ,shift))))))
        (with-gensyms (bytes)
          `(for ((,bytes (rep (unsigned-byte-8) ,(floor n 8) ,(floor n 8))))
             ,(ecase endian
                (:big `(bytes-unsigned-integer/be ,bytes))
                (:little `(bytes-unsigned-integer/le ,bytes))))))))

(declaim (inline unsigned-signed-integer))
(defun unsigned-signed-integer (nbits unsigned)
  (if (logbitp (1- nbits) unsigned)
      (- unsigned (ash 1 nbits))
      unsigned))

(defmethod expand-reader-type-expr ((name (eql 'signed-byte)) &rest args)
  (destructuring-bind (n) args
    (with-gensyms (unsigned)
      `(for ((,unsigned ,(expand-reader-type `(unsigned-byte ,n))))
         (the (signed-byte ,n) (unsigned-signed-integer ,n ,unsigned))))))

(defmacro define-unsigned-integer-parsers ()
  (loop :with mappings
        :for n :from 8 :to (max +word-size+ 64) :by 8
        :nconc (loop :for endian :in '(:little :big)
                     :for parser-name := (setf (assoc-value mappings (list `(unsigned-byte ,n) endian) :test #'equal)
                                               (intern (format nil "~A-~D~@[/~A~]" 'unsigned-byte n (when (> n 8) (ecase endian (:little 'le) (:big 'be))))))
                     :when (> n 8)
                       :collect (destructuring-ecase (unsigned-byte-parser n endian)
                                  ((for bindings &rest body)
                                   `(defparser ,parser-name ()
                                      (for ,bindings . ,body)))))
          :into parsers
        :finally
           (return
             (with-gensyms (name type endian)
               `(progn
                  (defun integer-type-parser (,type &optional (,endian +endian-default+))
                    (if-let ((,name (assoc-value ',mappings (list ,type ,endian) :test #'equal)))
                      (list ,name)
                      (progn
                        (assert (eq (first ,type) 'unsigned-byte))
                        (unsigned-byte-parser (second ,type) ,endian))))
                  ,@parsers)))))

(define-unsigned-integer-parsers)

(defmethod parsonic::expand-expr ((name (eql 'unsigned-byte)) &rest args)
  (parsonic::expand (integer-type-parser (cons name args))))

(defmethod parsonic::expand-expr ((name (eql 'signed-byte)) &rest args)
  (destructuring-bind (n) args
    (with-gensyms (unsigned)
      (parsonic::expand
       `(for ((,unsigned (unsigned-byte ,n)))
          (the (signed-byte ,n) (unsigned-signed-integer ,n ,unsigned)))))))

(defmethod parsonic::expand-expr ((name (eql 'bit)) &rest args)
  (declare (ignore args))
  (parsonic::expand-expr '(unsigned-byte 1)))

(defmethod expand-reader-type-expr ((name (eql 'unsigned-byte)) &rest args)
  (destructuring-bind (n) args
    (let* ((offset (prog1 *offset* (incf *offset* (/ n 8))))
           (binding (if (integerp offset)
                        (if (zerop (mod n 8))
                            (return-from expand-reader-type-expr (integer-type-parser (cons name args) *endian*))
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
                                 (expand-reader-type `(unsigned-byte ,(* bytes 8)))))))))))

(defmethod expand-reader-type-expr ((name (eql 'bit)) &rest args)
  (declare (ignore args))
  (expand-reader-type '(unsigned-byte 1)))
