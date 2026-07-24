(in-package #:binstruct)

#+sbcl
(define-compiler-macro unsigned-signed-integer (nbits unsigned)
  `(sb-c::mask-signed-field ,nbits ,unsigned))
