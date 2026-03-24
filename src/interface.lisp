(in-package #:binstruct)

(defvar *endian*)
(defvar *offset*)
(defvar *slots*)
(defvar *bindings*)

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

(defun slots-parser-bindings (slots)
  (let ((bindings (if (boundp '*bindings*) *bindings* t)))
    (prog1 (loop :for *slots* :on (copy-list slots)
                 :for ((name nil . options)) := *slots*
                 :for *bindings* := (let* ((bindings (when (consp bindings) (remove-if-not #'symbol-plist bindings :key #'car)))
                                           (*bindings* (append bindings *bindings*))
                                           (type (expand-type (getf options :type))))
                                      (nconc (nthcdr (length bindings) *bindings*) (list `(,name ,type))))
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
        (nconcf *bindings* bindings)))))

(defun expand-type-unit (type &key (endian :little) (offset 0))
  (when (and (boundp '*offset*) (boundp '*bindings*))
    (finish-partial-byte))
  (let ((*endian* endian)
        (*offset* offset)
        (*bindings* t))
    (with-gensyms (slot)
      `(let* ,(slots-parser-bindings `((,slot nil :type ,type)))
         (constantly ,slot)))))

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
             (defmethod expand-type-expr ((name (eql ',name)) &rest ,args)
               (destructuring-bind ,lambda-list ,args
                 `(for ((,',value ,(let ((*endian* ,endian)) (expand-type ',type))))
                    (,',unpack ,',value))))))))))

(defmacro defbinstruct (name-and-options lambda-list &rest slots)
  (destructuring-bind (name &rest options &aux (*package* (symbol-package name))) (ensure-list name-and-options)
    (destructuring-bind (&rest
                           args
                         &key
                           (type name typep)
                           (constructor (symbolicate '#:make- name))
                           (endian :little)
                           (include nil)
                         &allow-other-keys
                         &aux
                           (*endian* endian)
                           (*offset* 0))
        (mappend #'identity options)
      (delete-from-plistf args :constructor :include :endian)
      (labels ((slot-name (slot)
                 (getf slot :slot (car slot)))
               (excluded-slot-p (slot)
                 (or (let ((type (getf slot :type)))
                       (eq type 'position))
                     (null (slot-name slot))))
               (slots (&optional (slots (cons (car (ensure-list include)) slots)))
                 (loop :for slot :in slots
                       :when (symbolp slot)
                         :nconc (slots (get slot 'slots))
                       :when (consp slot)
                         :collect slot)))
        (let ((include (ensure-list include))
              (parser (symbolicate name '#:/parse))
              (constructor (symbolicate name '#:/construct))
              (derive (symbolicate name '#:/derive))
              (defstruct-constructor constructor))
          `(progn
             (eval-when (:compile-toplevel :load-toplevel :execute)
               ,(unless typep
                  `(defstruct (,name
                               (:constructor ,defstruct-constructor)
                               ,@(when include `((:include ,(car include))))
                               ,@(loop :for (key value) :on args :by #'cddr
                                       :collect (list key value)))
                     . ,(loop :for slot :in slots
                              :for (name initform . options) := slot
                              :unless (excluded-slot-p slot)
                                :collect (list* (getf slot :slot name) initform (nconc (remove-from-plist options :type) (list :type (lisp-type (getf options :type))))))))
               ,(with-gensyms (var args)
                  `(defmethod lisp-type-expr ((,var (eql ',name)) &rest ,args)
                     (declare (ignore ,var ,args))
                     ',type))
               (setf (get ',name 'slots) ',(cons (car include) slots)))
             ,(with-gensyms (next)
                (let* ((all-slots (delete-if-not #'slot-name (slots)))
                       (defstruct-slots (remove-if #'excluded-slot-p all-slots))
                       (ancestor-slots (when include (delete-if-not #'slot-name (slots (list (car include)))))))
                  `(progn
                     (defparser ,constructor ,(mapcar #'car all-slots)
                       (constantly
                        (progn
                          ,@(set-difference (mapcar #'car all-slots) (mapcar #'car defstruct-slots))
                          (the ,type (,defstruct-constructor
                                         . ,(loop :for slot :in defstruct-slots
                                                  :for (name) := slot
                                                  :nconc (list (make-keyword (getf slot :slot (car slot))) (car slot))))))))
                     (defparser ,parser (,next ,@(mapcar #'car ancestor-slots) ,@lambda-list)
                       (let* ,(slots-parser-bindings slots)
                         (parser-call ,next . ,(mapcar #'car all-slots))))
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
                       (,derive #',constructor . ,(parsonic::lambda-list-arguments lambda-list))))))))))))

(define-condition deserialize-error ()
  ((position :initarg :position :reader deserialize-error-position))
  (:report (lambda (condition stream)
             (format stream "Parse error at position ~A" (deserialize-error-position condition)))))

(defmacro defbinio ((name &rest lambda-list) type)
  (let ((reader (symbolicate '#:read- name)))
    (with-gensyms (input result error)
      `(defun ,reader (,input . ,lambda-list)
         (multiple-value-bind (,result ,error)
             (funcall
              (parser-lambda (,input)
                (declare (type ,(case type (stream 'parsonic::binary-input-stream) (t type)) ,input))
                (,name . ,(parsonic::lambda-list-arguments lambda-list)))
              ,input)
           (if ,error
               (error 'deserialize-error :position ,error)
               ,result))))))
