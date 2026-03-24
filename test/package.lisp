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
  (d 0 :type (unsigned-byte 4))
  (e 0 :type (signed-byte 9))
  (f 0 :type (signed-byte 16))
  (g 0 :type (unsigned-byte 7)))

(define-test bit-field :parent suite
  (is-parse-equal (bitfield-struct)
    (#(#b10011101 #b10010001 #b00000001 #b0000000 #b11111111)
      (make-bitfield-struct :a t :b 0 :c 3 :d 9 :e -111 :f -32768 :g 127))))

(defbinstruct padded-bitfield-struct ()
  (a nil :type (boolean (unsigned-byte 1)))
  (b 0 :type (unsigned-byte 1))
  (c 2 :type (magic (unsigned-byte 2)))
  (d (make-bitfield-struct) :type (bitfield-struct))
  (e 0 :type (signed-byte 9)))

(define-test padded-bitfield-struct :parent suite
  (is-parse-equal (padded-bitfield-struct)
    (#(#b00001011 #b10011101 #b10010001 #b00000001 #b0000000 #b11111111 #b00000000 #b00000001)
      (make-padded-bitfield-struct :a t :b 1 :c 2 :d (make-bitfield-struct :a t :b 0 :c 3 :d 9 :e -111 :f -32768 :g 127) :e -256))))

(defbinstruct base-string-struct ()
  (length 0 :type (unsigned-byte 32))
  (fixed-length "" :type (simple-base-string length))
  (zero-terminated "" :type terminated-base-string))

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

(defbinstruct (derived-struct (:include basic-struct)) (&optional (n 1))
  (p 0 :type position)
  (e (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (n)))
  (f 0 :type (signed-byte 16)))

(defbinstruct (derived-derived-struct (:include (derived-struct (1- n)))) (n)
  (g 0 :type (unsigned-byte 16))
  (h (make-array 0 :element-type '(signed-byte 8)) :type (custom
                                                          (simple-array (signed-byte 8) ((+ n (- 2 a) b)))
                                                          (lambda (array) (assert (= p 8)) array))))

(define-test derived-struct :parent suite
  (is-parse-equal (derived-struct)
    (#(#x01 #xFF #x02 #x01 #x00 #x00 #x00 #x80 #x7F #xFF #xFF)
      (make-derived-struct :a 1 :b -1 :c 258 :d -2147483648 :e (coerce #(127) '(simple-array (unsigned-byte 8) (*))) :f -1)))
  (is-parse-equal (derived-derived-struct 2)
    (#(#x01 #xFF #x02 #x01 #x00 #x00 #x00 #x80 #x7F #xFF #xFF #x34 #x12 #x80 #x7F #x7F)
      (make-derived-derived-struct :a 1 :b -1 :c 258 :d -2147483648 :e (coerce #(127) '(simple-array (unsigned-byte 8) (*)))
                                     :f -1 :g #x1234 :h (coerce #(-128 127) '(simple-array (signed-byte 8) (*)))))))

(defbinstruct empty-struct ())

(defbinstruct (derived-empty-struct (:include empty-struct)) ())

(define-test empty-struct :parent suite
  (locally (declare #+sbcl (sb-ext:muffle-conditions style-warning))
    (is-parse-equal (empty-struct)
      (#() (make-empty-struct)))
    (is-parse-equal (derived-empty-struct)
      (#() (make-derived-empty-struct)))))

(defbinstruct typed-struct (magic)
  (nil magic :type (magic (unsigned-byte 8))))

(defbinstruct (typed-struct-null (:include (typed-struct #x00))) ())

(defbinstruct (typed-struct-boolean (:include (typed-struct #x01))) ()
  (value 0 :type (boolean (unsigned-byte 8))))

(defbinstruct (typed-struct-integer (:include (typed-struct #x02))) ()
  (value 0 :type (signed-byte 32)))

(defbinstruct (typed-struct-string (:include (typed-struct #x03))) ()
  (value 0 :type terminated-base-string))

(defbinstruct or-struct ()
  (length 0 :type (unsigned-byte 8))
  (array (make-array 0 :element-type 'typed-struct) :type (simple-array
                                                           (or (typed-struct-null)
                                                               (typed-struct-boolean)
                                                               (typed-struct-integer)
                                                               (typed-struct-string))
                                                           (length))))

(define-test or-struct :parent suite
  (is-parse-equal (or-struct)
    (#(4
       #x00
       #x01 #x01
       #x02 #x78 #x56 #x34 #x12
       #x03 #x74 #x65 #x73 #x74 #x00)
      (make-or-struct
       :length 4
       :array (make-array
               4
               :element-type 'typed-struct
               :initial-contents
               (list
                (make-typed-struct-null)
                (make-typed-struct-boolean :value t)
                (make-typed-struct-integer :value #x12345678)
                (make-typed-struct-string :value (coerce "test" 'simple-base-string))))))))

(defbinstruct displaced-array-struct ()
  (length 0 :type (unsigned-byte 8))
  (array (make-array 0 :element-type '(unsigned-byte 8)) :type (array (unsigned-byte 8) (length))))

(define-test displaced-array :parent suite
  (let ((parser (parser-lambda (input) (declare (type (simple-array (unsigned-byte 8) (*)) input)) (displaced-array-struct))))
    (let* ((input (coerce #(4 #x12 #x34 #x56 #x78) '(simple-array (unsigned-byte 8) (*))))
           (array (displaced-array-struct-array (funcall parser input))))
      (is-values (array-displacement array) (eq input) (= 1))
      (is equalp (coerce #(#x12 #x34 #x56 #x78) '(simple-array (unsigned-byte 8) (*))) array))
    (true (nth-value 1 (funcall parser (coerce #(4 #x12 #x34 #x56) '(simple-array (unsigned-byte 8) (*))))))))

(defbinstruct list-struct ()
  (a (cons 0 0) :type (cons (unsigned-byte 4) (unsigned-byte 4)))
  (b 0 :type (unsigned-byte 1))
  (c 0 :type (list (unsigned-byte 7) (unsigned-byte 3)))
  (d 0 :type (unsigned-byte 5)))

(define-test list-struct :parent suite
  (is-parse-equal (list-struct)
    (#(#b11110000 #b10000001 #b10000111)
      (make-list-struct :a (cons 0 15) :b 1 :c (list 64 7) :d 16))))

(defbinstruct skip-struct ()
  (nil 0 :type (simple-array (unsigned-byte 8) (2)))
  (a 0 :type (unsigned-byte 8)))

(define-test skip-struct :parent suite
  (is-parse-equal (skip-struct)
    (#(0 0 12) (make-skip-struct :a 12))))

(defbinstruct parametric-type-struct (type)
  (start 0 :type position)
  (body nil :type (inline type))
  (end 0 :type position)
  (size 0 :type (custom null (constantly (- end start)))))

(defbinstruct test-parametric-type-struct ()
  (int (make-parametric-type-struct) :type (parametric-type-struct (unsigned-byte 8)))
  (string (make-parametric-type-struct) :type (parametric-type-struct terminated-base-string))
  (null (make-parametric-type-struct) :type (parametric-type-struct null)))

(define-test parametric-type-struct :parent suite
  (is-parse-equal (test-parametric-type-struct)
    (#(#x12 #x74 #x65 #x73 #x74 #x00)
      (make-test-parametric-type-struct
       :int (make-parametric-type-struct :body #x12 :size 1)
       :string (make-parametric-type-struct :body (coerce "test" 'simple-base-string) :size 5)
       :null (make-parametric-type-struct :body nil :size 0)))))
