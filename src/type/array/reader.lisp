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

(defmethod expand-reader-type-expr ((name (eql 'offset)) &rest args)
  (let ((*offset* 0))
    (apply #'expand-reader-type args)
    (throw 'offset *offset*)))

(defun type-offset (type)
  (catch 'offset (expand-reader-type-unit `(offset ,type))))

(defmethod expand-reader-type-expr ((name (eql 'capture-index)) &rest args)
  (destructuring-bind (index i type) args
    (with-gensyms (var)
      `(let ((,index (constantly (+ ,index ,i))))
         (declare (type non-negative-fixnum ,index) (ignorable ,index))
         (let* ,(slots-parser-bindings `((,var nil :type ,type)))
           (constantly ,var))))))

(defun expand-array-reader-type (array-type &rest array-type-args)
  (finish-reader-partial-byte)
  (destructuring-bind (element-type (dimension)) array-type-args
    (with-gensyms (array index length count)
      (let ((name (car (first *slots*))))
        (multiple-value-bind (bindings elements)
            (let ((*slots* (with-gensyms (slot) (list (list slot))))
                  (*place* (place-lambda (value) `(setf (aref ,array ,index) ,value)))
                  (*offset* 0)
                  (*bindings* t))
              (let* ((slots (loop :for i :below (denominator (type-offset element-type))
                                  :collect `(,(with-gensyms (element) element) nil :type (capture-index ,index ,i ,element-type))))
                     (results (mapcar #'first slots)))
                (values (slots-parser-bindings slots) results (assert (integerp *offset*)))))
          (if (equal element-type '(unsigned-byte 8))
              (if name
                  `(sequence/fixed-length (unsigned-byte-8) ,dimension ',(lisp-type (cons array-type array-type-args)))
                  `(skip ,dimension))
              `(let ((,length (constantly ,(case dimension (* 0) (t dimension)))))
                 (declare (type non-negative-fixnum ,length))
                 (let ((,count (constantly (ceiling ,length ,(length elements)))))
                   (declare (type non-negative-fixnum ,count))
                   ,(if name
                        `(let ((,array (constantly (make-array ,length :element-type ',(lisp-type element-type)
                                                                       :initial-element ,(type-default-value element-type)
                                                                       ,@(case dimension (* `(:fill-pointer 0 :adjustable t))))))
                               (,index (constantly 0)))
                           (declare (type (,(case dimension (* 'array) (t 'simple-array)) ,(lisp-type element-type) (*)) ,array)
                                    (type non-negative-fixnum ,index))
                           (for ((nil (rep (let* ,bindings
                                             ((lambda ()
                                                ,@(loop :for element :in elements
                                                        :for i :from 0
                                                        :collect `(,@(if (or (zerop i) (eq dimension '*)) '(progn) `(when (< ,index ,length)))
                                                                   ,(case dimension
                                                                      (* `(vector-push-extend ,element ,array))
                                                                      (t `(setf (aref ,array ,index) ,element)))
                                                                   (incf ,index)))
                                                (parser (constantly nil)))))
                                           ,count ,(case dimension (* most-positive-fixnum) (t count)))))
                             (setf ,array (coerce ,array '(,array-type ,(lisp-type element-type) (*))))))
                        `(progn (rep (let* ,bindings (constantly nil)) ,length ,length) (constantly nil)))))))))))

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
