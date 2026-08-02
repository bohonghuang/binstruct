(in-package #:binstruct)

(defmacro defbinenum (name-and-options lambda-list &body fields)
  (destructuring-bind (name &rest options &aux (*package* (symbol-package name))) (ensure-list name-and-options)
    (destructuring-bind (&key
                           (type '(unsigned-byte 32))
                           (endian +endian-default+ endianp)
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
        (with-gensyms (op value args)
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
               (for ((,value ,(handler-bind ((partial-byte-error (lambda (c) (declare (ignore c)) (invoke-restart 'pad))))
                                (expand-reader-type-unit type))))
                 (,unpack ,value)))
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (defmethod expand-reader-type-expr ((,op (eql ',name)) &rest ,args)
                 (destructuring-bind ,lambda-list ,args
                   `(for ((,',value ,(let ,(when endianp `((*endian* ,endian)))
                                       (expand-reader-type ',type))))
                      (,',unpack ,',value))))
               (defmethod expand-writer-type-expr ((,op (eql ',name)) &rest ,args)
                 (destructuring-bind ,lambda-list ,args
                   (let ((*value* `(,',pack ,,'*value*))
                         . ,(when endianp `((*endian* ,endian))))
                     (expand-writer-type ',type)))))))))))

(defmacro defbinstruct (name-and-options lambda-list &rest slots)
  (destructuring-bind (name &rest options &aux (*package* (symbol-package name))) (ensure-list name-and-options)
    (with-gensyms (next self null result)
      (destructuring-bind
          (&rest
             args
           &key
             (type name typep)
             (constructor (symbolicate '#:make- type) constructorp)
             (conc-name (symbolicate type '-))
             (endian +endian-default+)
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
                   (,derive #',constructor . ,(parsonic::lambda-list-arguments lambda-list))))
               ,(with-gensyms (output positions)
                  (let ((*output* output)
                        (*offset* 0)
                        (*inline* nil))
                    `(progn
                       (defun ,(emitter-name-symbol name) (,output ,self . ,lambda-list)
                         (declare (ignorable ,output ,self . ,(remove-if #'keywordp (parsonic::lambda-list-arguments lambda-list))))
                         (let ((,positions *positions*))
                           ,(when include
                              `(progn
                                 ,(let ((*value* self))
                                    (expand-writer-type include))
                                 (derive-pointer-positions ,positions)))
                           ,(let ((*bindings* (loop :with *package* := (symbol-package name)
                                                    :for slot :in all-slots
                                                    :for (slot-name slot-initform . slot-options) := slot
                                                    :when slot-name
                                                      :if (slot-excluded-p slot)
                                                        :collect `(,slot-name ,slot-initform) :into excluded
                                                      :else
                                                        :collect `(,slot-name (,(symbolicate (or conc-name '#:||) slot-name) ,self)) :into included
                                                    :finally (return (nconc included (stable-sort excluded #'< :key (compose #'char-code #'first-elt #'symbol-name #'car)))))))
                              `(let* ,*bindings*
                                 (declare (ignorable . ,(mapcar #'first *bindings*)))
                                 ,@(loop :for *slots* :on slots
                                         :for (slot) := *slots*
                                         :for (name initform . options) := slot
                                         :for *value* := (or name initform)
                                         :collect (expand-writer-type (getf options :type)))
                                 ,(finish-writer-partial-byte)))
                           (resolve-pointer-positions ,positions)))
                       ,(when *inline*
                          (with-gensyms (op args)
                            `(eval-when (:compile-toplevel :load-toplevel :execute)
                               (defmethod expand-writer-type-expr ((,op (eql ',name)) &rest ,args)
                                 (destructuring-bind ,lambda-list ,args
                                   (call-next-method
                                    ,op . ,(loop :for arg :in (parsonic::lambda-list-arguments lambda-list)
                                                 :if (member arg *inline*)
                                                   :collect (with-gensyms (output value)
                                                              `(with-gensyms (,output ,value)
                                                                 `(lambda (,,output ,,value)
                                                                    (declare (ignorable ,,output ,,value))
                                                                    ,(let ((*output* ,output) (*value* ,value))
                                                                       (expand-writer-type ,arg)))))
                                                 :else
                                                   :collect arg)))))))))))))))))

(define-condition deserialize-error (parse-error)
  ((input :initarg :input :reader deserialize-error-input)
   (position :initarg :position :reader deserialize-error-position)
   (info :initarg :info :reader deserialize-error-info))
  (:report (lambda (condition stream)
             (let ((info (deserialize-error-info condition)))
               (if (eq info 'parsonic::parse-failure)
                   (format stream "#~A(~A ~S ~D)" '#:s 'parsonic::parse-failure :position (deserialize-error-position condition))
                   (format stream "~A" info))))))

(define-condition unresolved-position-error (error)
  ((name :initarg :slot :reader unresolved-position-error-name))
  (:report (lambda (condition stream)
             (format stream "Unresolved position ~A" (unresolved-position-error-name condition)))))

(defmacro defbinio (type &optional (iotype t))
  (destructuring-bind (name &rest lambda-list) (ensure-list type)
    (let ((reader (symbolicate '#:read- name))
          (writer (symbolicate '#:write- name))
          (struct `(,name . ,(parsonic::lambda-list-arguments lambda-list))))
      (with-gensyms (input output value result error position vector)
        `(progn
           (defun ,reader (,input . ,lambda-list)
             (let ((*positions* nil))
               (multiple-value-bind (,result ,error)
                   (parser-run
                    (parser ,struct)
                    ,(case iotype
                       ((t) input)
                       ((stream) `(the parsonic::binary-input-stream ,input))
                       (otherwise `(the ,iotype ,input))))
                 (if ,error
                     (error 'deserialize-error :position ,error :input ,input :info ,result)
                     (if-let ((,position (find-if-not #'integerp *positions* :key #'cdr)))
                       (error 'unresolved-position-error :name (car ,position))
                       ,result)))))
           (defun ,writer (,output ,value . ,lambda-list)
             (let ((*positions* nil))
               ,(expand-writer-type-unit
                 struct
                 :endian '#:specified-by-type
                 :output (eswitch (iotype :test #'equal)
                           ('(simple-array (unsigned-byte 8) (*))
                             (prog1 `(etypecase ,output
                                       ((simple-array (unsigned-byte 8) (*))
                                        (multiple-value-bind (,result ,vector) (vector-emitter-output)
                                          (setf ,output (let ((,output ,output)) (lambda () (replace ,output ,vector))))
                                          ,result))
                                       ((array (unsigned-byte 8) (*))
                                        (multiple-value-bind (,result ,vector) (vector-emitter-output ,output)
                                          (setf ,output (constantly ,vector))
                                          ,result))
                                       (null
                                        (multiple-value-bind (,result ,vector) (vector-emitter-output)
                                          (setf ,output (constantly ,vector))
                                          ,result)))
                               (setf output `(funcall ,output))))
                           ('stream `(stream-emitter-output ,output))
                           ('t `(ensure-emitter-output ,output)))
                 :value value)
               (flush-pointer-positions)
               ,output)))))))
