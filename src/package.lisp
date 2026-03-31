(defpackage binstruct
  (:use #:cl #:alexandria #:parsonic)
  (:export
   #:defbinenum
   #:defbinstruct
   #:defbinio
   #:magic
   #:pointer
   #:terminated-base-string))

(in-package #:binstruct)
