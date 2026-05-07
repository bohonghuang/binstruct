(in-package #:binstruct)

(declaim (ftype (function (emitter-output simple-base-string)) emit-simple-base-string))
(defun emit-simple-base-string (output string)
  (loop :for char :of-type base-char :across string
        :do (funcall output (char-code char))))

(defmethod expand-writer-type-expr ((name (eql 'simple-base-string)) &rest args)
  (destructuring-bind (&optional (length '*)) args
    `(progn
       (emit-simple-base-string ,*output* ,*value*)
       ,(case length
          (* `(funcall ,*output* #x00))))))
