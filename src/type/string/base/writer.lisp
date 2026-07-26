(in-package #:binstruct)

(declaim (ftype (function (emitter-output simple-base-string &optional non-negative-fixnum)) emit-simple-base-string))
(defun emit-simple-base-string (output string &optional (length 0))
  (loop :for written :of-type non-negative-fixnum :from 0
        :for char :of-type base-char :across string
        :do (emitter-output-byte output (char-code char))
        :finally (loop :repeat (- length written) :do (emitter-output-byte output #x00))))

(defmethod expand-writer-type-expr ((name (eql 'simple-base-string)) &rest args)
  (destructuring-bind (&optional (length '*)) args
    (once-only (*value*)
      `(emit-simple-base-string
        ,*output*
        ,*value*
        ,(case length
           (* `(1+ (length ,*value*)))
           (t length))))))

(defmethod expand-writer-type-expr ((name (eql 'base-char)) &rest args)
  (declare (ignore args))
  `(emitter-output-byte ,*output* (char-code ,*value*)))

