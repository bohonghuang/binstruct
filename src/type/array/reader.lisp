(in-package #:binstruct)

(defparser sequence/fixed-length (element length &optional (type 'simple-array))
  (for ((list (rep element length length)))
    (declare (type list list))
    (unless (eq type 'null)
      (coerce list type))))

(defparser sequence/until-failure (element &optional (type 'simple-array))
  (for ((list (rep element)))
    (declare (type list list))
    (unless (eq type 'null)
      (coerce list type))))

(defparser skip (n)
  (let ((position (position)))
    (declare (type non-negative-fixnum position))
    (position (+ position n))))

(defun expand-array-reader-type (array-type &rest array-type-args)
  (finish-reader-partial-byte)
  (destructuring-bind (element-type (dimension)) array-type-args
    (with-gensyms (array index element length)
      (let ((name (car (first *slots*)))
            (parser (let ((*slots* nil) (*place* (place-lambda (value) `(setf (aref ,array ,index) ,value))))
                      (expand-reader-type-unit element-type))))
        (if (eq dimension '*)
            `(sequence/until-failure ,parser ',(lisp-type (cons array-type array-type-args)))
            (if (equal parser '(unsigned-byte-8))
                (if name
                    `(sequence/fixed-length ,parser ,dimension ',(lisp-type (cons array-type array-type-args)))
                    `(skip ,dimension))
                `(let ((,length (constantly ,dimension)))
                   (declare (type non-negative-fixnum ,length))
                   ,(if name
                        `(let ((,array (constantly (make-array ,length :element-type ',(lisp-type element-type) :initial-element ,(type-default-value element-type))))
                               (,index (constantly 0)))
                           (declare (type non-negative-fixnum ,index))
                           (rep ((lambda (,element)
                                   (setf (aref ,array ,index) ,element)
                                   (incf ,index)
                                   (parser (constantly nil)))
                                 ((lambda ()
                                    (let ((,index ,index))
                                      (declare (ignorable ,index))
                                      (parser ,parser)))))
                                ,length ,length)
                           (constantly ,array))
                        `(progn (rep ,parser ,length ,length) (constantly nil))))))))))

(defmethod parsonic::expand-expr ((name (eql 'simple-array)) &rest args)
  (destructuring-bind (type (length)) args
    (parsonic::expand
     (case length
       (* `(sequence/until-failure ,type ',(lisp-type (cons name args))))
       (t `(sequence/fixed-length ,type ,length ',(lisp-type (cons name args))))))))

(defmethod expand-reader-type-expr ((name (eql 'simple-array)) &rest args)
  (apply #'expand-array-reader-type name args))

(defmethod lisp-type-expr ((name (eql 'simple-array)) &rest args)
  (destructuring-bind (type (length)) args
    (declare (ignore length))
    `(simple-array ,(lisp-type type) (*))))

(defmethod parsonic::expand-expr ((name (eql 'array)) &rest args)
  (parsonic::expand `(simple-array . ,args)))

(defmethod expand-reader-type-expr ((name (eql 'array)) &rest args)
  (apply #'expand-array-reader-type name args))

(defmethod lisp-type-expr ((name (eql 'array)) &rest args)
  (cons name (cdr (apply #'lisp-type-expr 'simple-array args))))
