(defpackage binstruct.test
  (:import-from #:alexandria #:with-gensyms #:once-only #:rcurry)
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
                               (let ((binstruct::*positions* nil))
                                 (is equalp ,expected (parser-run ,eval ,bytes)))
                               (let ((binstruct::*positions* nil))
                                 (is equalp ,expected (funcall ,compiled ,bytes))))))))))

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

(define-test padded-bitfield :parent suite
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

(defbinstruct sentinel-terminated-array-element ()
  (nil #x12 :type (magic (unsigned-byte 8))))

(defbinstruct sentinel-terminated-array-struct ()
  (data (make-array 0 :element-type 'sentinel-terminated-array-element) :type (simple-array (sentinel-terminated-array-element) (*)))
  (end 0 :type (unsigned-byte 8)))

(define-test sentinel-terminated-array :parent suite
  (is-parse-equal (sentinel-terminated-array-struct)
    (#(#x12 #x12 #x12 #x00)
      (make-sentinel-terminated-array-struct
       :data (make-array 3
                         :element-type 'sentinel-terminated-array-element
                         :initial-contents (list (make-sentinel-terminated-array-element)
                                                 (make-sentinel-terminated-array-element)
                                                 (make-sentinel-terminated-array-element)))
       :end 0))))

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
  (h (make-array 0 :element-type '(signed-byte 8)) :type (map
                                                          (simple-array (signed-byte 8) ((+ n (- 2 a) b)))
                                                          (lambda (array) (assert (= p 8)) array))))

(define-test subtype :parent suite
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

(define-test or-type :parent suite
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

(define-test cons+list :parent suite
  (is-parse-equal (list-struct)
    (#(#b11110000 #b10000001 #b10000111)
      (make-list-struct :a (cons 0 15) :b 1 :c (list 64 7) :d 16))))

(defbinstruct skip-struct ()
  (nil 0 :type (simple-array (unsigned-byte 8) (2)))
  (a 0 :type (unsigned-byte 8)))

(define-test skip :parent suite
  (is-parse-equal (skip-struct)
    (#(0 0 12) (make-skip-struct :a 12))))

(defbinstruct parametric-type-struct (type)
  (start 0 :type position)
  (body nil :type (inline type))
  (end 0 :type position)
  (size 0 :type (map null (constantly (- end start)))))

(define-test parametric-type :parent suite
  (is-parse-equal (parametric-type-struct (unsigned-byte 8))
    (#(#x2A)
      (make-parametric-type-struct :body 42 :size 1)))
  (is-parse-equal (parametric-type-struct (map (unsigned-byte 8) #'1+ #'-))
    (#(#x2A)
      (make-parametric-type-struct :body 43 :size 1)))
  (is-parse-equal (parametric-type-struct (magic (unsigned-byte 8) #x2A))
    (#(#x2A)
      (make-parametric-type-struct :body 42 :size 1)))
  (is-parse-equal (parametric-type-struct (signed-byte 16))
    (#(#x34 #x12)
      (make-parametric-type-struct :body #x1234 :size 2)))
  (is-parse-equal (parametric-type-struct (unsigned-byte 32))
    (#(#x78 #x56 #x34 #x12)
      (make-parametric-type-struct :body #x12345678 :size 4)))
  (is-parse-equal (parametric-type-struct (boolean))
    (#(#x01)
      (make-parametric-type-struct :body t :size 1))
    (#(#x00)
      (make-parametric-type-struct :body nil :size 1)))
  (is-parse-equal (parametric-type-struct (simple-base-string 4))
    (#(#x74 #x65 #x73 #x74)
      (make-parametric-type-struct
       :body (coerce "test" 'simple-base-string)
       :size 4)))
  (is-parse-equal (parametric-type-struct (terminated-base-string))
    (#(#x74 #x65 #x73 #x74 #x00)
      (make-parametric-type-struct
       :body (coerce "test" 'simple-base-string)
       :size 5)))
  (is-parse-equal (parametric-type-struct (simple-array (unsigned-byte 8) (4)))
    (#(#x01 #x02 #x03 #x04)
      (make-parametric-type-struct
       :body (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4))
       :size 4)))
  (is-parse-equal (parametric-type-struct (enum-struct-enum))
    (#(#x01)
      (make-parametric-type-struct :body 'b :size 1)))
  (is-parse-equal (parametric-type-struct (basic-struct))
    (#(#xFF #xFF #xFF #x00 #xFF #xFF #xFF #xFF)
      (make-parametric-type-struct
       :body (make-basic-struct :a 255 :b -1 :c 255 :d -1)
       :size 8))))

(defbinstruct nonlocal-pointer-struct-1 ()
  (array (make-array 0 :element-type '(unsigned-byte 8)) :type (pointer (simple-array (unsigned-byte 8) (length)) (unsigned-byte 8) $nonlocal-base))
  (length 0 :type (unsigned-byte 8)))

(defbinstruct nonlocal-position-struct-1 ()
  (length 0 :type (unsigned-byte 8))
  (array (make-array 0 :element-type 'nonlocal-pointer-struct-1) :type (simple-array nonlocal-pointer-struct-1 (length)))
  ($nonlocal-base 0 :type position))

(defbinstruct nonlocal-pointer-struct-2 ()
  (length 0 :type (unsigned-byte 8))
  (array (make-array 0 :element-type 'simple-base-string) :type (simple-array (pointer terminated-base-string (unsigned-byte 8) $nonlocal-base) (length))))

(defbinstruct nonlocal-position-struct-2 ()
  (length 0 :type (unsigned-byte 8))
  (array (make-array 0 :element-type 'nonlocal-pointer-struct-2) :type (simple-array nonlocal-pointer-struct-2 (length)))
  ($nonlocal-base 0 :type position))

(defbinstruct nonlocal-pointer-struct-3 ()
  (value 0 :type (pointer (pointer (signed-byte 8) (unsigned-byte 8) $nonlocal-base-2) (unsigned-byte 8) $nonlocal-base-1)))

(defbinstruct nonlocal-position-struct-3-1 ()
  (length 0 :type (unsigned-byte 8))
  (array (make-array 0 :element-type 'nonlocal-pointer-struct-3) :type (simple-array nonlocal-pointer-struct-3 (length)))
  ($nonlocal-base-1 0 :type position)
  ($nonlocal-base-2 0 :type position))

(defbinstruct nonlocal-position-struct-3-2 ()
  (length 0 :type (unsigned-byte 8))
  (array (make-array 0 :element-type 'nonlocal-pointer-struct-3) :type (simple-array nonlocal-pointer-struct-3 (length)))
  ($nonlocal-base-2 0 :type position)
  ($nonlocal-base-1 0 :type position))

(defbinstruct nonlocal-pointer-struct-4 ()
  (length 0 :type (unsigned-byte 8))
  (array (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (pointer (pointer (unsigned-byte 8) (unsigned-byte 8) $nonlocal-base-2) (unsigned-byte 8) $nonlocal-base-1) (length))))

(defbinstruct nonlocal-position-struct-4-1 ()
  (length 0 :type (unsigned-byte 8))
  (array (make-array 0 :element-type 'nonlocal-pointer-struct-4) :type (simple-array nonlocal-pointer-struct-4 (length)))
  ($nonlocal-base-1 0 :type position)
  ($nonlocal-base-2 0 :type position))

(defbinstruct nonlocal-position-struct-4-2 ()
  (length 0 :type (unsigned-byte 8))
  (array (make-array 0 :element-type 'nonlocal-pointer-struct-4) :type (simple-array nonlocal-pointer-struct-4 (length)))
  ($nonlocal-base-2 0 :type position)
  ($nonlocal-base-1 0 :type position))

(define-test nonlocal-pointer :parent suite
  (is-parse-equal (nonlocal-position-struct-1)
    (#(#x03 #x00 #x03 #x01 #x03 #x02 #x03
       #x01 #x02 #x03 #x04 #x05)
      (make-nonlocal-position-struct-1
       :length 3
       :array (make-array 3 :element-type 'nonlocal-pointer-struct-1
                            :initial-contents (list (make-nonlocal-pointer-struct-1
                                                     :array (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3))
                                                     :length 3)
                                                    (make-nonlocal-pointer-struct-1
                                                     :array (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(2 3 4))
                                                     :length 3)
                                                    (make-nonlocal-pointer-struct-1
                                                     :array (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(3 4 5))
                                                     :length 3))))))
  (is-parse-equal (nonlocal-position-struct-2)
    (#(#x03 #x03 #x00 #x04 #x08 #x03 #x01 #x05 #x09 #x03 #x02 #x06 #x0A
       #x61 #x62 #x63 #x00 #x64 #x65 #x66 #x00 #x67 #x68 #x69 #x00)
      (make-nonlocal-position-struct-2
       :length 3
       :array (make-array 3 :element-type 'nonlocal-pointer-struct-2
                            :initial-contents (list (make-nonlocal-pointer-struct-2
                                                     :array (make-array 3 :element-type 'simple-base-string
                                                                          :initial-contents (mapcar (rcurry #'coerce 'simple-base-string) '("abc" "def" "ghi")))
                                                     :length 3)
                                                    (make-nonlocal-pointer-struct-2
                                                     :array (make-array 3 :element-type 'simple-base-string
                                                                          :initial-contents (mapcar (rcurry #'coerce 'simple-base-string) '("bc" "ef" "hi")))
                                                     :length 3)
                                                    (make-nonlocal-pointer-struct-2
                                                     :array (make-array 3 :element-type 'simple-base-string
                                                                          :initial-contents (mapcar (rcurry #'coerce 'simple-base-string) '("c" "f" "i")))
                                                     :length 3))))))
  (is-parse-equal (nonlocal-position-struct-3-1)
    (#1=#(#x03 #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08)
        (make-nonlocal-position-struct-3-1
         . #2=(:length 3
               :array (make-array 3 :element-type 'nonlocal-pointer-struct-3
                                    :initial-contents (list (make-nonlocal-pointer-struct-3
                                                             :value 6)
                                                            (make-nonlocal-pointer-struct-3
                                                             :value 7)
                                                            (make-nonlocal-pointer-struct-3
                                                             :value 8)))))))
  (is-parse-equal (nonlocal-position-struct-3-2)
    (#1# (make-nonlocal-position-struct-3-2 . #2#)))
  (is-parse-equal (nonlocal-position-struct-4-1)
    (#3=#(#x03 #x03 #x00 #x01 #x02 #x03 #x01 #x02 #x03 #x03 #x02 #x03 #x04
          #x05 #x05 #x06 #x06 #x07 #x01 #x02 #x03)
        (make-nonlocal-position-struct-4-1
         . #4=(:length 3
               :array (make-array 3 :element-type 'nonlocal-pointer-struct-4
                                    :initial-contents (list (make-nonlocal-pointer-struct-4
                                                             :length 3
                                                             :array (make-array 3 :element-type '(unsigned-byte 8)
                                                                                  :initial-contents '(1 1 2)))
                                                            (make-nonlocal-pointer-struct-4
                                                             :length 3
                                                             :array (make-array 3 :element-type '(unsigned-byte 8)
                                                                                  :initial-contents '(1 2 2)))
                                                            (make-nonlocal-pointer-struct-4
                                                             :length 3
                                                             :array (make-array 3 :element-type '(unsigned-byte 8)
                                                                                  :initial-contents '(2 2 3)))))))))
  (is-parse-equal (nonlocal-position-struct-4-2)
    (#3# (make-nonlocal-position-struct-4-2 . #4#))))
