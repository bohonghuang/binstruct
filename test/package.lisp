(defpackage binstruct.test
  (:import-from #:alexandria #:with-gensyms #:once-only #:rcurry)
  (:use #:cl #:parachute #:parsonic #:binstruct))

(in-package #:binstruct.test)

(define-test suite)

(defmacro is-rw (test struct &body tests)
  (with-gensyms (input output)
    (let ((reader-eval (make-symbol (format nil "~A [~A]" struct '#:reader/eval)))
          (reader-compiled (make-symbol (format nil "~A [~A]" struct '#:reader/compiled)))
          (writer-eval (make-symbol (format nil "~A [~A]" struct '#:writer-eval)))
          (writer-compiled (make-symbol (format nil "~A [~A]" struct '#:writer-compiled))))
      `(let* ((,reader-eval (lambda (,input) (parser-run (parser ,struct) ,input)))
              (,reader-compiled (lambda (,input) (parser-run (parser ,struct) (the (simple-array (unsigned-byte 8) (*)) ,input)))))
         ,@(loop :for (input expected) :in tests
                 :for bytes := (make-symbol (format nil "~A" input))
                 :collect (once-only (expected)
                            `(progn
                               (let ((,bytes (coerce ,input '(simple-array (unsigned-byte 8) (*)))))
                                 (let ((binstruct::*positions* nil))
                                   (is ,test ,expected (funcall ,reader-eval ,bytes)))
                                 (let ((binstruct::*positions* nil))
                                   (is ,test ,expected (funcall ,reader-compiled ,bytes)))
                                 (let ((,output (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
                                   (let ((binstruct::*positions* nil))
                                     ,(binstruct::expand-writer-type-unit struct :endian :little :output `(binstruct::vector-emitter-output ,output) :value expected)
                                     (binstruct::flush-pointer-positions))
                                   (let* ((,writer-eval (coerce ,output '(simple-array (unsigned-byte 8) (*))))
                                          (,writer-compiled ,writer-eval))
                                     (let ((binstruct::*positions* nil))
                                       (is ,test ,expected (funcall ,reader-eval ,writer-eval)))
                                     (let ((binstruct::*positions* nil))
                                       (is ,test ,expected (funcall ,reader-compiled ,writer-compiled)))))))))))))

(defmacro is-rw-equalp (struct &body tests)
  `(is-rw equalp ,struct . ,tests))

(defbinstruct basic-struct ()
  (a 0 :type (unsigned-byte 8))
  (b 0 :type (signed-byte 8))
  (c 0 :type (unsigned-byte 16))
  (d 0 :type (signed-byte 32)))

(define-test struct :parent suite
  (is-rw-equalp (basic-struct)
    (#(#xFF #xFF #xFF #x00 #xFF #xFF #xFF #xFF) (make-basic-struct :a 255 :b -1 :c 255 :d -1))))

(defbinstruct bigint-struct ()
  (a 0 :type (unsigned-byte 128)))

(define-test bigint :parent suite
  (is-rw-equalp (bigint-struct)
    (#(#xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF)
      (make-bigint-struct :a (1- (expt 2 128))))))

(defbinenum (big-endian-test-enum (:type (unsigned-byte 16))) ()
  alpha (beta #x1234) gamma)

(defbinstruct (big-endian-struct (:endian :big)) ()
  (a 0 :type (unsigned-byte 8))
  (b 0 :type (unsigned-byte 16))
  (c 0 :type (unsigned-byte 32))
  (d 0 :type (unsigned-byte 64))
  (e 0 :type (signed-byte 8))
  (f 0 :type (signed-byte 16))
  (g 0 :type (signed-byte 32))
  (h 0 :type (signed-byte 64))
  (i 'alpha :type big-endian-test-enum))

(define-test big-endian :parent suite
  (is-rw-equalp (big-endian-struct)
    (#(#xFF
       #x12 #x34
       #x12 #x34 #x56 #x78
       #x12 #x34 #x56 #x78 #x9A #xBC #xDE #xF0
       #xFF
       #xFF #xFF
       #xFF #xFF #xFF #xFF
       #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF
       #x12 #x34)
      (make-big-endian-struct
       :a #xFF
       :b #x1234
       :c #x12345678
       :d #x123456789ABCDEF0
       :e -1
       :f -1
       :g -1
       :h -1
       :i 'beta))))

(defbinenum (big-endian-enum (:type (unsigned-byte 16)) (:endian :big)) ()
  alpha (beta #x1234) gamma)

(defbinstruct big-endian-enum-struct ()
  (value 'alpha :type big-endian-enum))

(define-test big-endian-enum :parent suite
  (is-rw-equalp (big-endian-enum-struct)
    (#(#x12 #x34)
      (make-big-endian-enum-struct :value 'beta))))

(defbinstruct bitfield-struct ()
  (a nil :type (boolean (unsigned-byte 1)))
  (b 0 :type (unsigned-byte 1))
  (c 3 :type (satisfies (unsigned-byte 2)))
  (d 0 :type (unsigned-byte 4))
  (e 0 :type (signed-byte 9))
  (f 0 :type (signed-byte 16))
  (g 0 :type (unsigned-byte 7)))

(define-test bit-field :parent suite
  (is-rw-equalp (bitfield-struct)
    (#(#b10011101 #b10010001 #b00000001 #b0000000 #b11111111)
      (make-bitfield-struct :a t :b 0 :c 3 :d 9 :e -111 :f -32768 :g 127))))

(defbinstruct padded-bitfield-struct ()
  (a nil :type (boolean (unsigned-byte 1)))
  (b 0 :type (unsigned-byte 1))
  (c 2 :type (satisfies (unsigned-byte 2)))
  (d (make-bitfield-struct) :type (bitfield-struct))
  (e 0 :type (signed-byte 9)))

(define-test padded-bitfield :parent suite
  (is-rw-equalp (padded-bitfield-struct)
    (#(#b00001011 #b10011101 #b10010001 #b00000001 #b0000000 #b11111111 #b00000000 #b00000001)
      (make-padded-bitfield-struct :a t :b 1 :c 2 :d (make-bitfield-struct :a t :b 0 :c 3 :d 9 :e -111 :f -32768 :g 127) :e -256))))

(defbinstruct base-string-struct ()
  (length 0 :type (unsigned-byte 32))
  (fixed-length #.(coerce "" 'simple-string) :type (simple-base-string length))
  (zero-terminated #.(coerce "" 'simple-string) :type simple-base-string))

(define-test base-string :parent suite
  (is-rw-equalp (base-string-struct)
    (#(4 0 0 0 49 50 51 52 48 48 48 48 0)
      (make-base-string-struct
       :length 4
       :fixed-length (coerce "1234" 'simple-base-string) 
       :zero-terminated (coerce "0000" 'simple-base-string)))))

(defbinstruct utf-8-string-struct ()
  (length 0 :type (unsigned-byte 32))
  (fixed-length "" :type (simple-string length))
  (zero-terminated "" :type simple-string))

(define-test utf-8-string :parent suite
  (is-rw-equalp (utf-8-string-struct)
    (#(3 0 0 0 #xC3 #xA9 #xE4 #xB8 #x96 #xF0 #x9F #x98 #x80 #x00)
      (make-utf-8-string-struct
       :length 3
       :fixed-length (coerce "é世😀" 'simple-string)
       :zero-terminated (coerce "" 'simple-string))))
  (is-rw-equalp (utf-8-string-struct)
    (#(0 0 0 0 #x00)
      (make-utf-8-string-struct
       :length 0
       :fixed-length (coerce "" 'simple-string)
       :zero-terminated (coerce "" 'simple-string))))
  (is-rw-equalp (utf-8-string-struct)
    (#(5 0 0 0 #x68 #x65 #x6C #x6C #x6F #x68 #xC3 #xA9 #x6C #x6C #xC3 #xB6 #x00)
      (make-utf-8-string-struct
       :length 5
       :fixed-length (coerce "hello" 'simple-string)
       :zero-terminated (coerce "héllö" 'simple-string)))))

(defbinstruct base-char-struct ()
  (a #\a :type base-char)
  (b #\b :type base-char))

(define-test base-char :parent suite
  (is-rw-equalp (base-char-struct)
    (#(#x41 #x42) (make-base-char-struct :a #\A :b #\B))))

(defbinstruct character-struct ()
  (a #\a :type character)
  (b #\a :type character)
  (c #\a :type character)
  (d #\a :type character))

(define-test character :parent suite
  (is-rw-equalp (character-struct)
    (#(#x41 #xC3 #xA9 #xE4 #xB8 #x96 #xF0 #x9F #x98 #x80)
      (make-character-struct :a #\A :b #\é :c #\世 :d #\😀))))

(defun float= (a b)
  (etypecase (cons a b)
    ((cons single-float single-float)
     (= (float-features:single-float-bits a) (float-features:single-float-bits b)))
    ((cons double-float double-float)
     (= (float-features:double-float-bits a) (float-features:double-float-bits b)))))

(defbinstruct standard-float-struct ()
  (a 0.0s0 :type single-float)
  (b 0.0d0 :type double-float))

(defun standard-float-struct-equalp (a b)
  (and (float= (standard-float-struct-a a) (standard-float-struct-a b))
       (float= (standard-float-struct-b a) (standard-float-struct-b b))))

(defbinstruct custom-float-struct ()
  (a 0.0s0 :type (binstruct::ieee754-float 5 10))
  (b 0.0d0 :type (binstruct::ieee754-float 7 40)))

(defun custom-float-struct-equalp (a b)
  (and (float= (custom-float-struct-a a) (custom-float-struct-a b))
       (float= (custom-float-struct-b a) (custom-float-struct-b b))))

(define-test float :parent suite
  (is-rw-equalp (standard-float-struct)
    (#(#x00 #x00 #x80 #x3F #x00 #x00 #x00 #x00 #x00 #x00 #xF0 #x3F)
      (make-standard-float-struct :a 1.0s0 :b 1.0d0))
    (#(#x00 #x00 #x80 #xBF #x00 #x00 #x00 #x00 #x00 #x00 #xF0 #xBF)
      (make-standard-float-struct :a -1.0s0 :b -1.0d0))
    (#(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
      (make-standard-float-struct :a 0.0s0 :b 0.0d0))
    (#(#xC3 #xF5 #x48 #x40 #x1F #x85 #xEB #x51 #xB8 #x1E #x09 #x40)
      (make-standard-float-struct :a 3.14s0 :b 3.14d0)))
  (is-rw standard-float-struct-equalp (standard-float-struct)
    (#(#x00 #x00 #xC0 #x7F #x00 #x00 #x00 #x00 #x00 #x00 #xF8 #x7F)
      (make-standard-float-struct :a float-features:single-float-nan :b float-features:double-float-nan)))
  (is-rw-equalp (custom-float-struct)
    (#(#x00 #x3C #x00 #x00 #x00 #x00 #x00 #x3F)
      (make-custom-float-struct :a 1.0s0 :b 1.0d0))
    (#(#x00 #xBC #x00 #x00 #x00 #x00 #x00 #xBF)
      (make-custom-float-struct :a -1.0s0 :b -1.0d0))
    (#(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00)
      (make-custom-float-struct :a 0.0s0 :b 0.0d0))
    (#(#x00 #x3E #x00 #x00 #x00 #x00 #x80 #x3F)
      (make-custom-float-struct :a 1.5s0 :b 1.5d0)))
  (is-rw custom-float-struct-equalp (custom-float-struct)
    (#(#x01 #x7C #x01 #x00 #x00 #x00 #x00 #x7F)
      (make-custom-float-struct :a float-features:single-float-nan :b float-features:double-float-nan))))

(defbinstruct simple-array-struct ()
  (length 0 :type (unsigned-byte 8))
  (data (make-array 0 :element-type '(unsigned-byte 8)) 
        :type (simple-array (unsigned-byte 8) (length))))

(define-test simple-array :parent suite
  (is-rw-equalp (simple-array-struct)
    (#(4 1 2 3 4)
      (make-simple-array-struct 
       :length 4 
       :data (make-array 4 :element-type '(unsigned-byte 8) 
                           :initial-contents '(1 2 3 4))))))

(defbinstruct sentinel-terminated-array-element ()
  (nil #x12 :type (satisfies (unsigned-byte 8))))

(defbinstruct sentinel-terminated-array-struct ()
  (data (make-array 0 :element-type 'sentinel-terminated-array-element) :type (simple-array (sentinel-terminated-array-element) (*)))
  (end 0 :type (unsigned-byte 8)))

(define-test sentinel-terminated-array :parent suite
  (is-rw-equalp (sentinel-terminated-array-struct)
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
  (is-rw-equalp (enum-struct)
    (#(1) (make-enum-struct :value 'b))))

(defbinstruct tagged-union-struct ()
  (tag 'b :type enum-struct-enum)
  (data 0 :type (ecase tag
                  (a (simple-base-string 4))
                  (b (unsigned-byte 8))
                  (c (simple-array (unsigned-byte 8) (4))))))

(define-test tagged-union :parent suite
  (is-rw-equalp (tagged-union-struct)
    (#(0 116 101 115 116) (make-tagged-union-struct :tag 'a :data (coerce "test" 'simple-base-string)))
    (#(1 42) (make-tagged-union-struct :tag 'b :data 42))
    (#(2 1 2 3 4) (make-tagged-union-struct :tag 'c :data (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4))))))

(defbinstruct peek-struct ()
  (a 0 :type (peek (unsigned-byte 8)))
  (b 0 :type (unsigned-byte 8)))

(define-test peek :parent suite
  (is-rw-equalp (peek-struct)
    (#(42) (make-peek-struct :a 42 :b 42))))

(defbinstruct peek-position-struct ()
  (a 0 :type (peek (unsigned-byte 8) 1))
  (b 0 :type (unsigned-byte 8))
  (c 0 :type (unsigned-byte 8)))

(define-test peek-position :parent suite
  (is-rw-equalp (peek-position-struct)
    (#(42 7) (make-peek-position-struct :a 7 :b 42 :c 7))))

(defbinstruct pointer-struct ()
  (base 0 :type position)
  (array (make-array 0 :element-type '(unsigned-byte 8)) :type (pointer (simple-array (unsigned-byte 8) (length)) (unsigned-byte 8) base))
  (length 0 :type (unsigned-byte 8)))

(defbinstruct pointer-pointer-struct ()
  (base 0 :type position)
  (length 0 :type (unsigned-byte 8))
  (array (make-array 0 :element-type 'pointer-struct) :type (simple-array (pointer (pointer-struct) (unsigned-byte 8) base) (length))))

(define-test pointer :parent suite
  (is-rw-equalp (pointer-struct)
    (#(2 4 1 2 3 4)
      (make-pointer-struct
       :array (make-array 4 :element-type '(unsigned-byte 8)
                            :initial-contents '(1 2 3 4))
       :length 4)))
  (is-rw-equalp (pointer-pointer-struct)
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
  (h (make-array 0 :element-type '(signed-byte 8)) :type (satisfies
                                                          (simple-array (signed-byte 8) ((+ n (- 2 a) b)))
                                                          (lambda (array) (declare (ignore array)) (= p 8)))))

(define-test subtype :parent suite
  (is-rw-equalp (derived-struct)
    (#(#x01 #xFF #x02 #x01 #x00 #x00 #x00 #x80 #x7F #xFF #xFF)
      (make-derived-struct :a 1 :b -1 :c 258 :d -2147483648 :e (coerce #(127) '(simple-array (unsigned-byte 8) (*))) :f -1)))
  (is-rw-equalp (derived-derived-struct 2)
    (#(#x01 #xFF #x02 #x01 #x00 #x00 #x00 #x80 #x7F #xFF #xFF #x34 #x12 #x80 #x7F #x7F)
      (make-derived-derived-struct :a 1 :b -1 :c 258 :d -2147483648 :e (coerce #(127) '(simple-array (unsigned-byte 8) (*)))
                                   :f -1 :g #x1234 :h (coerce #(-128 127) '(simple-array (signed-byte 8) (*)))))))

(defbinstruct empty-struct ())

(defbinstruct (derived-empty-struct (:include empty-struct)) ())

(define-test empty-struct :parent suite
  (locally (declare #+sbcl (sb-ext:muffle-conditions style-warning))
    (is-rw-equalp (empty-struct)
      (#() (make-empty-struct)))
    (is-rw-equalp (derived-empty-struct)
      (#() (make-derived-empty-struct)))))

(defbinstruct typed-struct (magic)
  (nil magic :type (satisfies (unsigned-byte 8))))

(defbinstruct (typed-struct-null (:include (typed-struct #x00))) ())

(defbinstruct (typed-struct-boolean (:include (typed-struct #x01))) ()
  (value nil :type (boolean (unsigned-byte 8))))

(defbinstruct (typed-struct-integer (:include (typed-struct #x02))) ()
  (value 0 :type (signed-byte 32)))

(defbinstruct (typed-struct-string (:include (typed-struct #x03))) ()
  (value #.(coerce "" 'simple-base-string) :type simple-base-string))

(defbinstruct or-struct ()
  (length 0 :type (unsigned-byte 8))
  (array (make-array 0 :element-type 'typed-struct) :type (simple-array
                                                           (or (typed-struct-null)
                                                               (typed-struct-boolean)
                                                               (typed-struct-integer)
                                                               (typed-struct-string))
                                                           (length))))

(define-test or-type :parent suite
  (is-rw-equalp (or-struct)
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
  (let ((parser (lambda (input) (parser-run (parser (displaced-array-struct)) (the (simple-array (unsigned-byte 8) (*)) input)))))
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
  (is-rw-equalp (list-struct)
    (#(#b11110000 #b10000001 #b10000111)
      (make-list-struct :a (cons 0 15) :b 1 :c (list 64 7) :d 16))))

(defbinstruct skip-struct ()
  (nil #.(make-array 2 :element-type '(unsigned-byte 8) :initial-element 0) :type (simple-array (unsigned-byte 8) (2)))
  (a 0 :type (unsigned-byte 8)))

(define-test skip :parent suite
  (is-rw-equalp (skip-struct)
    (#(0 0 12) (make-skip-struct :a 12))))

(defbinstruct parametric-type-struct (type)
  (start 0 :type position)
  (body nil :type (inline type))
  (end 0 :type position)
  (size 0 :type (map null (constantly (- end start)))))

(define-test parametric-type :parent suite
  (is-rw-equalp (parametric-type-struct (unsigned-byte 8))
    (#(#x2A)
      (make-parametric-type-struct :body 42 :size 1)))
  (is-rw-equalp (parametric-type-struct (map (unsigned-byte 8) #'1+ #'1-))
    (#(#x2A)
      (make-parametric-type-struct :body 43 :size 1)))
  (is-rw-equalp (parametric-type-struct (satisfies (unsigned-byte 8) (alexandria:curry #'= #x2A)))
    (#(#x2A)
      (make-parametric-type-struct :body 42 :size 1)))
  (is-rw-equalp (parametric-type-struct (signed-byte 16))
    (#(#x34 #x12)
      (make-parametric-type-struct :body #x1234 :size 2)))
  (is-rw-equalp (parametric-type-struct (unsigned-byte 32))
    (#(#x78 #x56 #x34 #x12)
      (make-parametric-type-struct :body #x12345678 :size 4)))
  (is-rw-equalp (parametric-type-struct (boolean))
    (#(#x01)
      (make-parametric-type-struct :body t :size 1))
    (#(#x00)
      (make-parametric-type-struct :body nil :size 1)))
  (is-rw-equalp (parametric-type-struct (simple-base-string 4))
    (#(#x74 #x65 #x73 #x74)
      (make-parametric-type-struct
       :body (coerce "test" 'simple-base-string)
       :size 4)))
  (is-rw-equalp (parametric-type-struct (simple-base-string))
    (#(#x74 #x65 #x73 #x74 #x00)
      (make-parametric-type-struct
       :body (coerce "test" 'simple-base-string)
       :size 5)))
  (is-rw-equalp (parametric-type-struct (simple-array (unsigned-byte 8) (4)))
    (#(#x01 #x02 #x03 #x04)
      (make-parametric-type-struct
       :body (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4))
       :size 4)))
  (is-rw-equalp (parametric-type-struct (enum-struct-enum))
    (#(#x01)
      (make-parametric-type-struct :body 'b :size 1)))
  (is-rw-equalp (parametric-type-struct (basic-struct))
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
  (array (make-array 0 :element-type 'simple-base-string) :type (simple-array (pointer simple-base-string (unsigned-byte 8) $nonlocal-base) (length))))

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
  (is-rw-equalp (nonlocal-position-struct-1)
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
  (is-rw-equalp (nonlocal-position-struct-2)
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
  (is-rw-equalp (nonlocal-position-struct-3-1)
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
  (is-rw-equalp (nonlocal-position-struct-3-2)
    (#1# (make-nonlocal-position-struct-3-2 . #2#)))
  (is-rw-equalp (nonlocal-position-struct-4-1)
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
  (is-rw-equalp (nonlocal-position-struct-4-2)
    (#3# (make-nonlocal-position-struct-4-2 . #4#))))

(defbinstruct position-pointer-struct ()
  (start 0 :type position)
  (string 0 :type (pointer simple-base-string (unsigned-byte 8) $base))
  ($base 0 :type (pointer position (unsigned-byte 8) start)))

(define-test position-pointer :parent suite
  (is-rw-equalp (position-pointer-struct)
    (#(4 2 0 0 0 0 72 101 108 108 111 0) (make-position-pointer-struct :string (coerce "Hello" 'simple-base-string)))))

(defbinstruct local-pointers-in-cons-struct ()
  (base 0 :type position)
  (cons '(0 . 0) :type (cons (pointer (unsigned-byte 8) (unsigned-byte 4) base)
                             (pointer (unsigned-byte 8) (unsigned-byte 4) base))))

(defbinstruct nonlocal-pointers-in-cons-struct ()
  ($base1 0 :type position)
  (cons '(0 . 0) :type (cons (pointer (unsigned-byte 8) (unsigned-byte 4) $base1)
                             (pointer (unsigned-byte 8) (unsigned-byte 4) $base2)))
  ($base2 0 :type position))

(define-test pointer-list :parent suite
  (is-rw-equalp (local-pointers-in-cons-struct)
    (#(#x21 #x01 #x02)
      (make-local-pointers-in-cons-struct
       :cons (cons 1 2))))
  (is-rw-equalp (nonlocal-pointers-in-cons-struct)
    (#(#x11 #x01 #x02)
      (make-nonlocal-pointers-in-cons-struct
       :cons (cons 1 2)))))

(defbinstruct local-pointers-in-tagged-union-struct ()
  (base 0 :type position)
  (tag 0 :type (unsigned-byte 8))
  (length nil :type (ecase tag
                      (0 (null))
                      (1 (unsigned-byte 8))))
  (data 0 :type (ecase tag
                  (0 (unsigned-byte 8))
                  (1 (pointer (simple-array (unsigned-byte 8) (length)) (unsigned-byte 8) base)))))

(defbinstruct nonlocal-pointers-in-tagged-union-struct ()
  ($base1 0 :type position)
  (tag 0 :type (unsigned-byte 8))
  (length nil :type (ecase tag
                      (0 (null))
                      (1 (pointer (unsigned-byte 8) (unsigned-byte 8) $base1))))
  (data 0 :type (ecase tag
                  (0 (unsigned-byte 8))
                  (1 (pointer (simple-array (unsigned-byte 8) (length)) (unsigned-byte 8) $base2))))
  ($base2 0 :type position))

(define-test pointer-union :parent suite
  (is-rw-equalp (local-pointers-in-tagged-union-struct)
    (#(0 42)
      (make-local-pointers-in-tagged-union-struct :tag 0 :data 42))
    (#(1 4 3 1 2 3 4)
      (make-local-pointers-in-tagged-union-struct
       :tag 1 :length 4
       :data (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4)))))
  (is-rw-equalp (nonlocal-pointers-in-tagged-union-struct)
    (#(0 42)
      (make-nonlocal-pointers-in-tagged-union-struct :tag 0 :data 42))
    (#(1 3 1 4 1 2 3 4)
      (make-nonlocal-pointers-in-tagged-union-struct
       :tag 1 :length 4
       :data (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4))))))
