(in-package #:binstruct)

(defun resolve-pointer-position (name)
  (when-let ((cons (assoc name *positions*)))
    (setf (car cons) (funcall (lastcar (cdr cons)))
          (cdr cons) (nbutlast (cdr cons)))))

(defun resolve-pointer-positions (&optional end (predicate (complement #'global-position-p)))
  (let ((positions *positions*))
    (when (eq positions end)
      (return-from resolve-pointer-positions positions))
    (loop :for current-positions :on positions
          :for (cons) := current-positions
          :for (name . handlers) := cons
          :until (eq current-positions end)
          :if (and (symbolp name) (funcall predicate name))
            :collect (cons (funcall (lastcar handlers)) (cons (constantly name) (nbutlast handlers))) :into current
          :else
            :unless (eql name -1)
              :collect cons :into previous
          :finally (setf *positions* (nconc current (list (cons -1 (list #'values))) previous current-positions)))))

(defun derive-pointer-positions (&optional end)
  (let ((positions *positions*))
    (when (eq positions end)
      (return-from derive-pointer-positions positions))
    (loop :for current-positions :on positions
          :for (cons) := current-positions
          :for (position . handlers) := cons
          :until (eq current-positions end)
          :until (= position -1)
          :do (setf (car cons) (funcall (first handlers))
                    (cdr cons) (nconc (cdr handlers) (list (constantly position)))))))

(defun flush-pointer-positions ()
  (resolve-pointer-positions nil #'values)
  (loop :while *positions*
        :do (loop :for (position . handlers) :in (shiftf *positions* nil)
                  :do (loop :for handler :in handlers
                            :do (funcall handler position)))))

(defmethod expand-writer-type-expr ((name (eql 'peek)) &rest args)
  (declare (ignore args)))

(defmethod expand-writer-type-expr ((name (eql 'position)) &rest args)
  (declare (ignore args))
  (when-let ((name (slot-name (first *slots*))))
    (with-gensyms (position)
      `(let ((,position (setf ,name (emitter-output-position ,*output*))))
         ,(when (global-position-p name)
            `(resolve-pointer-position ',name))
         (push (cons ',name (list (constantly ,position))) *positions*)))))

(defmethod expand-writer-type-expr ((name (eql 'pointer)) &rest args)
  (destructuring-bind (data-type pointer-type &optional (base 0)) args
    (with-gensyms (position offset)
      (once-only (*value*)
        `(let ((,position (emitter-output-position ,*output*)))
           (push
            (lambda (,offset)
              (let ((,position (shiftf (emitter-output-position ,*output*) ,position)))
                ,(expand-writer-type-unit pointer-type :value `(- ,position ,offset))
                (setf (emitter-output-position ,*output*) ,position))
              ,(expand-writer-type-unit data-type))
            (assoc-value *positions* ',base))
           ,(let ((*value* (type-default-value pointer-type)))
              (expand-writer-type pointer-type)))))))
