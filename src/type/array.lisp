(in-package #:binstruct)

(defparser sequence (element length &optional (type 'simple-array))
  (for ((list (rep element (max 0 length) (if (minusp length) most-positive-fixnum length))))
    (declare (type list list))
    (unless (eq type 'null)
      (coerce list type))))

(defparser skip (n)
  (let ((position (position)))
    (declare (type non-negative-fixnum position))
    (position (+ position n))))

(defun expand-array-type (array-type &rest array-type-args)
  (finish-partial-byte)
  (destructuring-bind (element-type (length)) array-type-args
    (with-gensyms (array index element)
      (let ((name (car (first *slots*)))
            (parser (let ((*slots* nil) (*place* (place-lambda (value) `(setf (aref ,array ,index) ,value))))
                      (expand-type-unit element-type))))
        (if (eq length '*)
            `(sequence ,parser -1 ',(lisp-type (cons array-type array-type-args)))
            (if (equal parser '(unsigned-byte-8))
                (if name
                    `(sequence ,parser ,length ',(lisp-type (cons array-type array-type-args)))
                    `(skip ,length))
                (if name
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
                    `(progn (rep ,parser ,length ,length) (constantly nil)))))))))

(defmethod parsonic::expand-expr ((name (eql 'simple-array)) &rest args)
  (destructuring-bind (type (length)) args
    (parsonic::expand `(sequence ,type ,length ',(lisp-type (cons name args))))))

(defmethod expand-type-expr ((name (eql 'simple-array)) &rest args)
  (apply #'expand-array-type name args))

(defmethod lisp-type-expr ((name (eql 'simple-array)) &rest args)
  (destructuring-bind (type (length)) args
    (declare (ignore length))
    `(simple-array ,(lisp-type type) (*))))

(defmethod parsonic::expand-expr ((name (eql 'array)) &rest args)
  (destructuring-bind (type (length)) args
    (parsonic::expand `(sequence ,type ,length ',(lisp-type (cons name args))))))

(defmethod expand-type-expr ((name (eql 'array)) &rest args)
  (apply #'expand-array-type name args))

(defmethod lisp-type-expr ((name (eql 'array)) &rest args)
  (cons name (cdr (apply #'lisp-type-expr 'simple-array args))))
