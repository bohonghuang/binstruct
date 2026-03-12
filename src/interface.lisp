(in-package #:binstruct)

(defgeneric expand-type-expr (name &rest args)
  (:method (name &rest args)
    (cons name args)))

(defun expand-type (desc)
  (apply #'expand-type-expr (ensure-list desc)))

(defgeneric lisp-type-expr (name &rest args)
  (:method (name &rest args)
    (if args (cons name args) name)))

(defun lisp-type (type)
  (apply #'lisp-type-expr (ensure-list type)))

(defvar *endian*)
(defvar *offset*)
(defvar *slots*)

(defun slots-parser-bindings (slots)
  (loop :with *slots* := (loop :for slot :in slots
                               :collect (etypecase slot
                                          (list (copy-list slot))
                                          (symbol `(,slot 0 :type (position)))))
        :with byte-var := nil
        :for (slot) := (or *slots* (loop-finish))
        :for offset := *offset*
        :for type := (expand-type (ensure-list (getf (cddr slot) :type)))
        :for (slot-name initform . slot-options) := slot
        :when (and (= offset *offset*) (not (integerp *offset*)))
          :collect `(nil ,(expand-type `(unsigned-byte ,(* (- (ceiling *offset*) *offset*) 8))))
        :when (and (integerp offset) (not (integerp *offset*)))
          :collect (setf byte-var `(byte ,offset))
        :when (and (not (integerp offset)) (integerp *offset*))
          :do (setf (second byte-var) (let ((bytes (- *offset* (second byte-var)))
                                            (*offset* 0))
                                        (check-type bytes fixnum)
                                        (expand-type `(unsigned-byte ,(* bytes 8))))
                    byte-var nil)
        :collect `(,slot-name ,type)
        :do (setf *slots* (cdr *slots*))
        :when (and (null *slots*) (not (integerp *offset*)))
          :do (setf *slots* `((nil nil :type (unsigned-byte ,(* (- (ceiling *offset*) *offset*) 8)))))))

(defun expand-type-unit (type &key (endian :little) (offset 0))
  (let ((*endian* endian)
        (*offset* offset))
    (with-gensyms (slot)
      `(let* ,(slots-parser-bindings `((,slot nil :type ,type)))
         (constantly ,slot)))))

(defmacro defbinenum (name-and-options lambda-list &body fields)
  (destructuring-bind (name &rest options) (ensure-list name-and-options)
    (destructuring-bind (&key
                           (type '(unsigned-byte 32))
                           (endian :little)
                         &allow-other-keys
                         &aux
                           (*endian* endian))
        (mappend #'identity options)
      (let ((pack (intern (format nil "~A-~A" name 'integer)))
            (unpack (intern (format nil "~A-~A" 'integer name)))
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
  (destructuring-bind (name &rest options) (ensure-list name-and-options)
    (destructuring-bind (&rest
                           args
                         &key
                           (type name typep)
                           (constructor (intern (format nil "~A-~A" '#:make name)))
                           (endian :little)
                         &allow-other-keys
                         &aux
                           (*endian* endian)
                           (*offset* 0))
        (mappend #'identity options)
      (delete-from-plistf args :constructor :endian)
      (flet ((excluded-slot-p (slot)
               (or (let ((type (getf slot :type)))
                     (eq type 'position))
                   (null (getf slot :slot (car slot))))))
        `(progn
           ,(unless typep
              `(defstruct (,name (:constructor ,constructor) . ,options)
                 . ,(loop :for slot :in slots
                          :for (name initform . options) := slot
                          :unless (excluded-slot-p slot)
                            :collect (list* (getf slot :slot name) initform (nconc (remove-from-plist options :type) (list :type (lisp-type (getf options :type))))))))
           (defparser ,name ,lambda-list
             (let* ,(slots-parser-bindings slots)
               (constantly (the ,type (,constructor . ,(loop :for slot :in slots
                                                             :for (name) := slot
                                                             :unless (excluded-slot-p slot)
                                                               :nconc (list (make-keyword (getf slot :slot (car slot))) (car slot))))))))
           ,(with-gensyms (type args)
              `(defmethod lisp-type-expr ((,type (eql ',name)) &rest ,args)
                 (declare (ignore ,args))
                 ,type)))))))
