(in-package #:binstruct)

(defvar *endian*)
(defvar *offset*)
(defvar *slots*)
(defvar *bindings*)
(defvar *place*)
(defvar *positions*)

(deftype input-position ()
  'non-negative-fixnum)

(defun finish-partial-byte ()
  (unless (integerp *offset*)
    (nconcf *bindings* (list `(nil ,(expand-type `(unsigned-byte ,(* (- (ceiling *offset*) *offset*) 8))))))))

(defgeneric expand-type-expr (name &rest args)
  (:method (name &rest args)
    (finish-partial-byte)
    (cons name args)))

(defun expand-type (desc)
  (apply #'expand-type-expr (ensure-list desc)))

(defgeneric lisp-type-expr (name &rest args)
  (:method (name &rest args)
    (if args (cons name args) name)))

(defun lisp-type (type)
  (apply #'lisp-type-expr (ensure-list type)))

(defun slot-name (&optional (slot (first *slots*)))
  (car slot))

(defun slot-excluded-p (&optional (slot (first *slots*)))
  (or (eq (lisp-type (getf slot :type)) 'input-position)
      (null (slot-name slot))))

(defun slots-parser-bindings (slots &aux (bindings (if (boundp '*bindings*) *bindings* t)))
  (prog1 (loop :for *slots* :on (copy-tree slots)
               :for (slot) := *slots*
               :for *bindings* := (let* ((*place* (place-lambda (value) `(progn ,(place-set (place-parent) value) (setf ,(slot-name) ,value))))
                                         (bindings (when (consp bindings) (remove-if-not #'symbol-plist bindings :key #'car)))
                                         (*bindings* (append bindings *bindings*))
                                         (type (expand-type (getf slot :type))))
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

(defun expand-type-unit (type &key (endian :little) (offset 0))
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

(defmacro defbinenum (name-and-options lambda-list &body fields)
  (destructuring-bind (name &rest options &aux (*package* (symbol-package name))) (ensure-list name-and-options)
    (destructuring-bind (&key
                           (type '(unsigned-byte 32))
                           (endian :little)
                         &allow-other-keys
                         &aux
                           (*endian* endian))
        (mappend #'identity options)
      (let ((pack (symbolicate name '#:- 'integer))
            (unpack (symbolicate 'integer '#:- name))
            (fields (loop :for field :in fields
                          :for (name value) := (ensure-list field)
                          :for integer := (or value 0) :then (or value (1+ integer))
                          :collect (list name integer))))
        (with-gensyms (value args)
          `(progn
             (deftype ,name ()
               '(member . ,(mapcar #'first fields)))
             (declaim (ftype (function (,name) (values ,type)) ,pack))
             (defun ,pack (,value)
               (ecase ,value . ,(loop :for (name value) :in fields
                                      :collect `(,name ',value))))
             (declaim (ftype (function (,type) (values ,name)) ,unpack))
             (defun ,unpack (,value)
               (ecase ,value . ,(loop :for (name value) :in fields
                                      :collect `(,value ',name))))
             (defparser ,name ,lambda-list
               (for ((,value ,(expand-type-unit type)))
                 (,unpack ,value)))
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (defmethod expand-type-expr ((name (eql ',name)) &rest ,args)
                 (destructuring-bind ,lambda-list ,args
                   `(for ((,',value ,(let ((*endian* ,endian)) (expand-type ',type))))
                      (,',unpack ,',value)))))))))))

(defparser inline (parser)
  parser)

(defmethod lisp-type-expr ((name (eql 'inline)) &rest args)
  (declare (ignore args))
  t)

(defmacro defbinstruct (name-and-options lambda-list &rest slots)
  (destructuring-bind (name &rest options &aux (*package* (symbol-package name))) (ensure-list name-and-options)
    (with-gensyms (next self null result)
      (destructuring-bind
          (&rest
             args
           &key
             (type name typep)
             (constructor (symbolicate '#:make- type) constructorp)
             (endian :little)
             (include nil)
           &allow-other-keys
           &aux
             (*endian* endian)
             (*offset* 0)
             (*place* (place-lambda (value)
                        `(unless (eq ,self ',null)
                           ,(cond
                              ((slot-excluded-p) value)
                              ((and (or (not typep) (subtypep type 'structure-object)) (not constructorp))
                               `(setf (,(symbolicate type '- (slot-name)) ,self) ,value))
                              (t (place-set (place-null) value)))))))
          (mappend #'identity options)
        (delete-from-plistf args :constructor :include :endian)
        (labels ((slots (&optional (slots (cons (car (ensure-list include)) slots)))
                   (loop :for slot :in slots
                         :when (symbolp slot)
                           :nconc (slots (get slot 'slots))
                         :when (consp slot)
                           :collect slot)))
          (let* ((include (ensure-list include))
                 (parser (symbolicate name '#:/parse))
                 (defstruct-constructor constructor)
                 (constructor (symbolicate name '#:/construct))
                 (derive (symbolicate name '#:/derive))
                 (all-slots (delete-duplicates (delete-if-not #'slot-name (slots)) :key #'car))
                 (all-defstruct-slots (remove-if #'slot-excluded-p all-slots))
                 (defstruct-slots (delete-duplicates (delete-if #'slot-excluded-p (copy-list slots)) :key #'car))
                 (ancestor-slots (when include (delete-duplicates (delete-if-not #'slot-name (slots (list (car include)))) :key #'car)))
                 (bindings (slots-parser-bindings slots)))
            `(progn
               (eval-when (:compile-toplevel :load-toplevel :execute)
                 ,(unless typep
                    `(defstruct (,name
                                 (:constructor ,defstruct-constructor)
                                 ,@(when include `((:include ,(car include))))
                                 ,@(loop :for (key value) :on args :by #'cddr
                                         :collect (list key value)))
                       . ,(loop :for slot :in defstruct-slots
                                :for (name initform . options) := slot
                                :collect (list* (getf slot :slot name) initform (nconc (remove-from-plist options :type) (list :type (lisp-type (getf options :type))))))))
                 ,(with-gensyms (var args)
                    `(defmethod lisp-type-expr ((,var (eql ',name)) &rest ,args)
                       (declare (ignore ,var ,args))
                       ',type))
                 (setf (get ',name 'slots) ',(cons (car include) slots)))
               (progn
                 (defparser ,constructor ,(mapcar #'car all-slots)
                   (constantly
                    (progn
                      ,@(set-difference (mapcar #'car all-slots) (mapcar #'car all-defstruct-slots))
                      (the ,type (,defstruct-constructor
                                     . ,(loop :for slot :in all-defstruct-slots
                                              :for (name) := slot
                                              :nconc (list (make-keyword (getf slot :slot (car slot))) (car slot))))))))
                 (defparser ,parser (,next ,@(mapcar #'car ancestor-slots) ,@lambda-list)
                   ,(if (place-used-p *place*)
                        `(let* ((,self (constantly ',null)) . ,bindings)
                           (for ((,result (parser-call ,next . ,(mapcar #'car all-slots))))
                             (setf ,self ,result)))
                        `(let* ,bindings (parser-call ,next . ,(mapcar #'car all-slots)))))
                 (defparser ,derive (,next . ,lambda-list)
                   ,(if include
                        `(,(let ((*package* (symbol-package (car include))))
                             (symbolicate (car include) '#:/derive))
                          ,(if-let ((args (parsonic::lambda-list-arguments lambda-list)))
                             `(rcurry (curry #',parser ,next) . ,args)
                             `(curry #',parser ,next))
                          . ,(cdr include))
                        `(,parser ,next . ,(parsonic::lambda-list-arguments lambda-list))))
                 (defparser ,name ,lambda-list
                   (,derive #',constructor . ,(parsonic::lambda-list-arguments lambda-list)))))))))))

(define-condition deserialize-error (parse-error)
  ((input :initarg :input :reader deserialize-error-input)
   (position :initarg :position :reader deserialize-error-position))
  (:report (lambda (condition stream)
             (format stream "Parse error at position ~A" (deserialize-error-position condition)))))

(define-condition unresolved-position-error (error)
  ((name :initarg :slot :reader unresolved-position-error-name))
  (:report (lambda (condition stream)
             (format stream "Unresolved position ~A" (unresolved-position-error-name condition)))))

(defmacro defbinio (type iotype)
  (destructuring-bind (name &rest lambda-list) (ensure-list type)
    (let ((reader (symbolicate '#:read- name)))
      (with-gensyms (input result error position)
        `(defun ,reader (,input . ,lambda-list)
           (let ((*positions* nil))
             (multiple-value-bind (,result ,error)
                 (funcall
                  (parser-lambda (,input)
                    (declare (type ,(case iotype (stream 'parsonic::binary-input-stream) (t iotype)) ,input))
                    (,name . ,(parsonic::lambda-list-arguments lambda-list)))
                  ,input)
               (if ,error
                   (error 'deserialize-error :position ,error :input ,input)
                   (if-let ((,position (find-if-not #'integerp *positions* :key #'cdr)))
                     (error 'unresolved-position-error :name (car ,position))
                     ,result)))))))))
