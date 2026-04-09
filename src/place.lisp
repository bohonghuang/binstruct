(in-package #:binstruct)

(defun place-null ()
  (constantly '(assert nil)))

(defmacro place-lambda ((value &rest lambda-list) &body body)
  (with-gensyms (place-parent place-used-p)
    `(let ((,place-parent (or (when (boundp '*place*) *place*) (place-null)))
           (,place-used-p nil))
       (flet ((place-parent () ,place-parent))
         (declare (ignorable #'place-parent))
         (lambda (,value . ,lambda-list)
           (case ,value
             (place-used-p ,place-used-p)
             (place-parent ,place-parent)
             (t (setf ,place-used-p t) . ,body)))))))

(defun place-used-p (place)
  (funcall place 'place-used-p))

(defun place-parent (place)
  (funcall place 'place-parent))

(defun place-set (place value)
  (funcall place value))
