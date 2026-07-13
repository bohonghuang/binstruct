(defpackage binstruct
  (:use #:cl #:alexandria #:parsonic)
  (:import-from #:buffered-streams #:copy-cons)
  (:export
   #:defbinenum
   #:defbinstruct
   #:defbinio
   #:pointer))

(in-package #:binstruct)
