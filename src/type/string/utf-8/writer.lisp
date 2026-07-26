(in-package #:binstruct)

(declaim (ftype (function (emitter-output character)) emit-utf-8-character)
         (inline emit-utf-8-character))
(defun emit-utf-8-character (output char)
  (let ((code (char-code char)))
    (declare (type (integer 0 #x10FFFF) code))
    (cond ((< code #x80)
           (emitter-output-byte output code))
          ((< code #x800)
           (emitter-output-byte output (logior #b11000000 (ldb (byte 5 6) code)))
           (emitter-output-byte output (logior #b10000000 (ldb (byte 6 0) code))))
          ((< code #x10000)
           (emitter-output-byte output (logior #b11100000 (ldb (byte 4 12) code)))
           (emitter-output-byte output (logior #b10000000 (ldb (byte 6 6) code)))
           (emitter-output-byte output (logior #b10000000 (ldb (byte 6 0) code))))
          (t
           (emitter-output-byte output (logior #b11110000 (ldb (byte 3 18) code)))
           (emitter-output-byte output (logior #b10000000 (ldb (byte 6 12) code)))
           (emitter-output-byte output (logior #b10000000 (ldb (byte 6 6) code)))
           (emitter-output-byte output (logior #b10000000 (ldb (byte 6 0) code)))))))

(declaim (ftype (function (emitter-output simple-string &optional non-negative-fixnum)) emit-simple-string))
(defun emit-simple-string (output string &optional (length 0))
  (loop :for char :of-type character :across string :do (emit-utf-8-character output char))
  (loop :repeat (- length (length string)) :do (emit-utf-8-character output #\Null)))

(defmethod expand-writer-type-expr ((name (eql 'simple-string)) &rest args)
  (destructuring-bind (&optional (length '*)) args
    (once-only (*value*)
      `(emit-simple-string
        ,*output*
        ,*value*
        ,(case length
           (* `(1+ (length ,*value*)))
           (t length))))))

(defmethod expand-writer-type-expr ((name (eql 'character)) &rest args)
  (declare (ignore args))
  `(emit-utf-8-character ,*output* ,*value*))

