(defpackage binstruct
  (:use #:cl #:alexandria #:parsonic)
  (:export
   #:defbinenum
   #:defbinstruct
   #:magic
   #:pointer
   #:terminated-base-string
   #:custom))

(in-package #:binstruct)
