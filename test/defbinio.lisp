(in-package #:binstruct.test)

(defmacro defbinalias (name-and-options lambda-list parser)
  (destructuring-bind (name &rest options) (alexandria:ensure-list name-and-options)
    (destructuring-bind (&key (type name typep)) (alexandria:mappend #'identity options)
      (unless typep
        (let ((inferred-type (binstruct::lisp-type parser)))
          (unless (eq inferred-type t)
            (setf type inferred-type))))
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (setf (fdefinition ',(alexandria:symbolicate name '- 'value)) (fdefinition 'identity)))
         (defbinstruct (,name (:type ,type) (:constructor progn))
           ,lambda-list
           (value nil :type ,parser))))))

(defbinalias basic-struct/stream () (basic-struct))
(defbinalias basic-struct/u8vector () (basic-struct))
(defbinalias derived-struct/stream (&optional (n 1)) (derived-struct n))
(defbinalias derived-struct/u8vector (&optional (n 1)) (derived-struct n))

(defbinio (basic-struct/stream) stream)
(defbinio (basic-struct/u8vector) (simple-array (unsigned-byte 8) (*)))
(defbinio (derived-struct/stream n) stream)
(defbinio (derived-struct/u8vector n) (simple-array (unsigned-byte 8) (*)))

(define-test defbinio :parent suite
  (define-test stream
    (let* ((struct (make-basic-struct :a 255 :b -1 :c 255 :d -1))
           (out-stream (flex:make-in-memory-output-stream))
           (result-stream (write-basic-struct/stream out-stream struct))
           (stream-bytes (flex:get-output-stream-sequence result-stream)))
      (is equalp struct (read-basic-struct/stream (flex:make-in-memory-input-stream stream-bytes))))
    (let* ((struct (make-derived-struct :a 1 :b -1 :c 258 :d -2147483648 :e (coerce #(127) '(simple-array (unsigned-byte 8) (*))) :f -1))
           (out-stream (flex:make-in-memory-output-stream))
           (result-stream (write-derived-struct/stream out-stream struct 1))
           (stream-bytes (flex:get-output-stream-sequence result-stream)))
      (is equalp struct (read-derived-struct/stream (flex:make-in-memory-input-stream stream-bytes) 1))))
  (define-test u8vector
    (let* ((struct (make-basic-struct :a 255 :b -1 :c 255 :d -1))
           (bytes (write-basic-struct/u8vector nil struct)))
      (is equalp struct (read-basic-struct/u8vector (coerce bytes '(simple-array (unsigned-byte 8) (*))))))
    (let* ((struct (make-derived-struct :a 1 :b -1 :c 258 :d -2147483648 :e (coerce #(127) '(simple-array (unsigned-byte 8) (*))) :f -1))
           (bytes (write-derived-struct/u8vector nil struct 1)))
      (is equalp struct (read-derived-struct/u8vector (coerce bytes '(simple-array (unsigned-byte 8) (*))) 1)))))
