(in-package #:binstruct)

(declaim (ftype (function (writer-output simple-base-string)) write-simple-base-string))
(defun write-simple-base-string (output string)
  (loop :for char :of-type base-char :across string
        :do (funcall output (char-code char))))

(defmethod expand-write-type-expr ((name (eql 'simple-base-string)) &rest args)
  (destructuring-bind (&optional (length '*)) args
    `(progn
       (write-simple-base-string ,*output* ,*value*)
       ,(case length
          (* `(funcall ,*output* #x00))))))
