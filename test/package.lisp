(defpackage binstruct.test
  (:import-from #:alexandria #:with-gensyms #:once-only)
  (:use #:cl #:parachute #:parsonic #:binstruct))

(in-package #:binstruct.test)

(define-test suite)

(defmacro is-parse-equal (parser &body tests)
  (with-gensyms (bytes)
    (let ((eval (make-symbol (format nil "~A [~A]" parser 'eval)))
          (compiled (make-symbol (format nil "~A [~A]" parser 'compiled))))
      `(let ((,eval (parser ,parser))
             (,compiled (parser-lambda (,bytes) (declare (type (simple-array (unsigned-byte 8) (*)) ,bytes)) ,parser)))
         ,@(loop :for (input expected) :in tests
                 :for bytes := (make-symbol (format nil "~A" input))
                 :collect (once-only (expected)
                            `(let ((,bytes (coerce ,input '(simple-array (unsigned-byte 8) (*)))))
                               (is equalp ,expected (parser-run ,eval ,bytes))
                               (is equalp ,expected (funcall ,compiled ,bytes)))))))))

(defbinstruct basic-struct ()
  (a 0 :type (unsigned-byte 8))
  (b 0 :type (signed-byte 8))
  (c 0 :type (unsigned-byte 16))
  (d 0 :type (signed-byte 32)))

(define-test struct :parent suite
  (is-parse-equal (basic-struct)
    (#(#xFF #xFF #xFF #x00 #xFF #xFF #xFF #xFF) (make-basic-struct :a 255 :b -1 :c 255 :d -1))))

(defbinstruct bigint-struct ()
  (a 0 :type (unsigned-byte 128)))

(define-test bigint :parent suite
  (is-parse-equal (bigint-struct)
    (#(#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF)
      (make-bigint-struct :a (1- (expt 2 128))))))

(defbinstruct bitfield-struct ()
  (a nil :type (boolean (unsigned-byte 1)))
  (b 0 :type (unsigned-byte 1))
  (c 3 :type (magic (unsigned-byte 2)))
  (d 0 :type (unsigned-byte 4)))

(define-test bit-field :parent suite
  (is-parse-equal (bitfield-struct)
    (#(#b10011101) (make-bitfield-struct :a t :b 0 :c 3 :d 9))))

(defbinstruct base-string-struct ()
  (length 0 :type (unsigned-byte 32))
  (fixed-length "" :type (simple-base-string length))
  (zero-terminated "" :type (terminated-base-string '#x00)))

(define-test base-string :parent suite
  (is-parse-equal (base-string-struct)
    (#(4 0 0 0 49 50 51 52 48 48 48 48 0)
      (make-base-string-struct
       :length 4
       :fixed-length (coerce "1234" 'simple-base-string) 
       :zero-terminated (coerce "0000" 'simple-base-string)))))

(defbinstruct simple-array-struct ()
  (length 0 :type (unsigned-byte 8))
  (data (make-array 0 :element-type '(unsigned-byte 8)) 
        :type (simple-array (unsigned-byte 8) (length))))

(define-test simple-array :parent suite
  (is-parse-equal (simple-array-struct)
    (#(4 1 2 3 4)
      (make-simple-array-struct 
       :length 4 
       :data (make-array 4 :element-type '(unsigned-byte 8) 
                           :initial-contents '(1 2 3 4))))))

(defbinenum (enum-struct-enum (:type (unsigned-byte 8))) ()
  a (b 1) c)

(defbinstruct enum-struct ()
  (value 'a :type enum-struct-enum))

(define-test enum :parent suite
  (is-parse-equal (enum-struct)
    (#(1) (make-enum-struct :value 'b))))

(defbinstruct tagged-union-struct ()
  (tag 'a :type enum-struct-enum)
  (data "" :type (ecase tag
                   (a (simple-base-string 4))
                   (b (unsigned-byte 8))
                   (c (simple-array (unsigned-byte 8) (4))))))

(define-test tagged-union :parent suite
  (is-parse-equal (tagged-union-struct)
    (#(0 116 101 115 116) (make-tagged-union-struct :tag 'a :data (coerce "test" 'simple-base-string)))
    (#(1 42) (make-tagged-union-struct :tag 'b :data 42))
    (#(2 1 2 3 4) (make-tagged-union-struct :tag 'c :data (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4))))))

(defbinstruct pointer-struct ()
  (base 0 :type position)
  (array (make-array 0 :element-type '(unsigned-byte 8)) :type (pointer (simple-array (unsigned-byte 8) (length)) (unsigned-byte 8) base))
  (length 0 :type (unsigned-byte 8)))

(defbinstruct pointer-pointer-struct ()
  (base 0 :type position)
  (length 0 :type (unsigned-byte 8))
  (array (make-array 0 :element-type 'pointer-struct) :type (simple-array (pointer (pointer-struct) (unsigned-byte 8) base) (length))))

(define-test pointer :parent suite
  (is-parse-equal (pointer-struct)
    (#(2 4 1 2 3 4)
      (make-pointer-struct
       :array (make-array 4 :element-type '(unsigned-byte 8)
                            :initial-contents '(1 2 3 4))
       :length 4)))
  (is-parse-equal (pointer-pointer-struct)
    (#(4 8 16 8 16 0 0 0
       4 4 0 0 1 2 3 4
       2 6 1 2 3 4 5 6)
      (make-pointer-pointer-struct
       :length 4
       :array (make-array
               4
               :element-type 'pointer-struct
               :initial-contents (let ((list (list (make-pointer-struct
                                                    :array (make-array 4 :element-type '(unsigned-byte 8)
                                                                         :initial-contents '(1 2 3 4))
                                                    :length 4)
                                                   (make-pointer-struct
                                                    :array (make-array 6 :element-type '(unsigned-byte 8)
                                                                         :initial-contents '(1 2 3 4 5 6))
                                                    :length 6))))
                                   (append list list)))))))
