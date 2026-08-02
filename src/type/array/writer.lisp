(in-package #:binstruct)

(defmethod expand-writer-type-expr ((name (eql 'array)) &rest args)
  (destructuring-bind (element-type (length)) args
    (declare (ignore length))
    (with-gensyms (length index default)
      `(progn
         ,(finish-writer-partial-byte)
         ,(once-only (*value* *output*)
            (let ((emitters (loop :with *offset* := 0
                                  :and value := `(aref ,*value* ,index)
                                  :for *value* := value :then `(if (< ,index ,length) ,value ,default)
                                  :collect `(progn ,(expand-writer-type element-type) (incf ,index))
                                  :until (integerp *offset*))))
              `(loop :with ,length :of-type non-negative-fixnum := (length ,*value*)
                     :and ,index :of-type non-negative-fixnum := 0
                     ,@(when (> (length emitters) 1) `(:and ,default := ,(type-default-value element-type)))
                     :while (< ,index ,length)
                     :do (progn . ,emitters))))))))

(defmethod expand-writer-type-expr ((name (eql 'simple-array)) &rest args)
  (apply #'expand-writer-type-expr 'array args))
