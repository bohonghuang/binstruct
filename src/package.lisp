(defpackage binstruct
  (:use #:cl #:alexandria #:parsonic)
  (:import-from #:float-features
   #:bits-single-float #:bits-double-float
   #:single-float-bits #:double-float-bits)
  (:import-from #:buffered-streams #:copy-cons)
  (:export
   #:defbinenum
   #:defbinstruct
   #:defbinio
   #:pointer))

(in-package #:binstruct)
