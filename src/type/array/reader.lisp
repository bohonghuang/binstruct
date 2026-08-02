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
    (with-gensyms (array index length count)
      (let ((name (car (first *slots*))))
        (multiple-value-bind (parsers bindings)
            (loop :with *slots* := (with-gensyms (slot) (list (list slot)))
                  :and *place* := (place-lambda (value) `(setf (aref ,array ,index) ,value))
                  :and *offset* := 0
                  :and *bindings* := nil
                  :collect (expand-reader-type-unit element-type) :into parsers
                  :until (integerp *offset*)
                  :finally (return (values parsers *bindings*)))
          (if (equal parsers '((unsigned-byte-8)))
              (if name
                  `(sequence/fixed-length ,(car parsers) ,dimension ',(lisp-type (cons array-type array-type-args)))
                  `(skip ,dimension))
              `(let ((,length (constantly ,(case dimension (* 0) (t dimension)))))
                 (declare (type non-negative-fixnum ,length))
                 (let ((,count (constantly (ceiling ,length ,(length parsers)))))
                   (declare (type non-negative-fixnum ,count))
                   ,(if name
                        `(let ((,array (constantly (make-array ,length :element-type ',(lisp-type element-type)
                                                                       :initial-element ,(type-default-value element-type)
                                                                       ,@(case dimension (* `(:fill-pointer 0 :adjustable t))))))
                               (,index (constantly 0)))
                           (declare (type (,(case dimension (* 'array) (t 'simple-array)) ,(lisp-type element-type) (*)) ,array)
                                    (type non-negative-fixnum ,index))
                           (for ((nil (rep (let ,bindings
                                             (,(let ((elements (loop :repeat (length parsers) :collect (with-gensyms (element) element))))
                                                 `(lambda ,elements
                                                    ,@(loop :for element :in elements
                                                            :for i :from 0
                                                            :collect `(,@(if (or (zerop i) (eq dimension '*)) '(progn) `(when (< ,index ,length)))
                                                                       ,(case dimension
                                                                          (* `(vector-push-extend ,element ,array))
                                                                          (t `(setf (aref ,array ,index) ,element)))
                                                                       (incf ,index)))
                                                    (parser (constantly nil))))
                                              . ,(loop :for parser :in parsers
                                                       :for i :from 0
                                                       :collect `((lambda ()
                                                                    (let ((,index (+ ,index ,i)))
                                                                      (declare (ignorable ,index))
                                                                      (parser ,parser)))))))
                                           ,count ,(case dimension (* most-positive-fixnum) (t count)))))
                             (setf ,array (coerce ,array '(,array-type ,(lisp-type element-type) (*))))))
                        `(progn (rep (let ,bindings . ,parsers) ,length ,length) (constantly nil)))))))))))

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
