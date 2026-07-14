(in-package #:binstruct)

(defun push-pointer-position (name position)
  (when (global-position-p name)
    (when-let ((cons (assoc name *positions*)))
      (let ((last (last (cdr cons))))
        (if (eq (car last) #'values)
            (return-from push-pointer-position
              (setf (car last) (constantly position)
                    (cdr cons) (copy-cons (cdr cons))))
            (setf (car cons) (funcall (car last))
                  (cdr cons) (cons (constantly name) (nbutlast (cdr cons)))))))
    (push (cons name (list (constantly position))) *positions*)))

(defun local-pointer-position (name)
  (loop :for (pointer . handlers) :in *positions*
        :while (symbolp pointer)
        :when (eq pointer name)
          :return (funcall (lastcar handlers))))

(defun resolve-pointer-positions (&optional end (predicate (complement #'global-position-p)))
  (let ((positions *positions*))
    (when (eq positions end)
      (return-from resolve-pointer-positions))
    (loop :for current-positions :on positions
          :for (cons) := current-positions
          :for (pointer . handlers) := cons
          :until (eq current-positions end)
          :if (and (symbolp pointer) (funcall predicate pointer))
            :if (funcall (lastcar handlers))
              :collect (cons pointer (last handlers)) :into next
              :and :collect (cons (funcall (lastcar handlers)) (cons (constantly pointer) (nbutlast handlers))) :into current
            :else
              :collect cons :into next
            :end
          :else
            :unless (eql pointer -1)
              :collect cons :into previous
          :finally
             (setf *positions* (nconc current (list (cons -1 (list #'values))) previous current-positions))
             (return next))))

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
  (loop
    (loop :with next := (resolve-pointer-positions nil #'values)
          :for (position . handlers) :in (shiftf *positions* (mapcar #'copy-cons next))
          :do (loop :for handler :in handlers
                    :do (funcall handler position))
          :finally
             (when (loop :for (pointer . handlers) :in *positions*
                         :always (eq (assoc-value next pointer) handlers))
               (return-from flush-pointer-positions)))))

(defmethod expand-writer-type-expr ((name (eql 'peek)) &rest args)
  (declare (ignore args)))

(defmethod expand-writer-type-expr ((name (eql 'position)) &rest args)
  (declare (ignore args))
  (when-let ((name (slot-name (first *slots*))))
    `(push-pointer-position ',name (setf ,name (emitter-output-position ,*output*)))))

(defmethod expand-writer-type-expr ((name (eql 'pointer)) &rest args)
  (destructuring-bind (data-type pointer-type &optional (base 0) (offset *offset*) (start (get *output* 'offset))) args
    (unless (global-position-p base)
      (when-let ((binding (assoc base *bindings*)))
        (setf (second binding) `(local-pointer-position ',base))))
    (with-gensyms (output position)
      (once-only (*value*)
        `(let* ((,output ,*output*)
                (,position (emitter-output-position (ensure-emitter-output ,output))))
           ,(let ((*value* (type-default-value pointer-type)))
              (expand-writer-type pointer-type))
           ,(let ((n (- *offset* offset))
                  (output (if (get *output* 'offset) *output* output)))
              (once-only (output)
                (with-gensyms (offset handlers)
                  `(let ((,handlers (push
                                     (lambda (,offset)
                                       (let ((,position (shiftf (emitter-output-position ,*output*) ,position)))
                                         ,(let* ((*value* `(- ,position ,offset))
                                                 (delta (when-let ((start (or start (get *output* 'offset))))
                                                          (- (- *offset* n) start)))
                                                 (*offset* (- n))
                                                 (start (when delta (- *offset* delta)))
                                                 (*output* output))
                                            (setf (get output 'offset) start)
                                            (expand-writer-type pointer-type))
                                         (setf (emitter-output-position ,*output*) ,position))
                                       ,(expand-writer-type-unit data-type :slots *slots*))
                                     ,(if (global-position-p base)
                                          `(assoc-value *positions* ',base)
                                          `(assoc-value *positions* ,base)))))
                     (unless (cdr ,handlers)
                       (setf (cdr ,handlers) (list #'values))))))))))))
