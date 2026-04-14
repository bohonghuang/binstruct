(in-package #:binstruct)

(defun finish-partial-byte ()
  (unless (integerp *offset*)
    (nconcf *bindings* (list `(nil ,(expand-reader-type `(unsigned-byte ,(* (- (ceiling *offset*) *offset*) 8))))))))

(defgeneric expand-reader-type-expr (name &rest args)
  (:method (name &rest args)
    (finish-partial-byte)
    (cons name args)))

(defun expand-reader-type (desc)
  (apply #'expand-reader-type-expr (ensure-list desc)))

(defun slots-parser-bindings (slots &aux (bindings (if (boundp '*bindings*) *bindings* t)))
  (prog1 (loop :for *slots* :on (copy-tree slots)
               :for (slot) := *slots*
               :for *bindings* := (let* ((*place* (place-lambda (value) `(progn ,(place-set (place-parent) value) (setf ,(slot-name) ,value))))
                                         (bindings (when (consp bindings) (remove-if-not #'symbol-plist bindings :key #'car)))
                                         (*bindings* (append bindings *bindings*))
                                         (type (expand-reader-type (getf slot :type))))
                                    (nconc (nthcdr (length bindings) *bindings*)
                                           (if (place-used-p *place*)
                                               (list `(,(car slot) (constantly nil))
                                                     (with-gensyms (value)
                                                       `(nil ((lambda (,value)
                                                                (setf ,(car slot) ,value)
                                                                (parser (constantly nil)))
                                                              ,type))))
                                               (list `(,(car slot) ,type)))))
               :finally
                  (if (listp bindings)
                      (loop :for (var val) :in *bindings*
                            :for pvar := (gensym (string '#:pvar))
                            :when (setf (symbol-plist pvar) (symbol-plist var))
                              :collect `(,pvar (constantly nil)) :into parent-bindings
                              :and :collect `(nil (constantly (setf ,pvar ,var))) :into new-bindings
                            :finally
                               (setf bindings parent-bindings)
                               (nconcf *bindings* new-bindings))
                      (finish-partial-byte))
                  (return *bindings*))
    (when (consp bindings)
      (nconcf *bindings* bindings))))

(defun expand-reader-type-unit (type &key (endian :little) (offset 0))
  (when (and (boundp '*offset*) (boundp '*bindings*))
    (finish-partial-byte))
  (with-gensyms (unit null value setter)
    (let ((*endian* endian)
          (*offset* offset)
          (*bindings* t)
          (*slots* (or (when (boundp '*slots*) *slots*) (with-gensyms (slot) (list (list slot)))))
          (*place* (place-lambda (value) `(unless (eq ,unit ',null) (funcall ,setter ,value)))))
      (let* ((name (slot-name)) (bindings (slots-parser-bindings `((,name nil :type ,type)))))
        (cond
          ((place-used-p *place*)
           `(let* ((,setter (constantly (lambda (,value) ,(place-set (place-parent *place*) value))))
                   (,unit (constantly ',null)) . ,bindings)
              (constantly (setf ,unit ,name))))
          ((> (length bindings) 1)
           `(let* ,bindings (constantly ,name)))
          (t (assert (eq (first (first bindings)) name))
             (second (first bindings))))))))
