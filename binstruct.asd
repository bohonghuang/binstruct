(defsystem binstruct
  :version "0.1.0"
  :author "Bohong Huang <bohonghuang@qq.com>"
  :maintainer "Bohong Huang <bohonghuang@qq.com>"
  :license "Apache-2.0"
  :description "Declarative binary structure encoding and decoding for Common Lisp."
  :homepage "https://github.com/bohonghuang/binstruct"
  :bug-tracker "https://github.com/bohonghuang/binstruct/issues"
  :source-control (:git "https://github.com/bohonghuang/binstruct.git")
  :depends-on (#:alexandria #:float-features #:parsonic)
  :pathname "src/"
  :components ((:file "package")
               (:file "common" :depends-on ("package"))
               (:file "place" :depends-on ("package"))
               (:file "reader" :depends-on ("package" "common" "place"))
               (:file "writer" :depends-on ("package"))
               (:file "macro" :depends-on ("package" "common" "reader" "place" "writer"))
               (:module "type"
                :components ((:file "default")
                             (:module "condition"
                              :components ((:file "reader")
                                           (:file "writer"))
                              :depends-on ("integer"))
                             (:module "integer"
                              :components ((:file "reader")
                                           (:file "reader-optimize" :depends-on ("reader"))
                                           (:file "writer")))
                             (:module "boolean"
                              :components ((:file "reader")
                                           (:file "writer"))
                              :depends-on ("integer"))
                             (:module "float"
                              :components ((:file "reader")
                                           (:file "writer"))
                              :depends-on ("integer"))
                             (:module "pointer"
                              :components ((:file "reader")
                                           (:file "writer"))
                              :depends-on ("integer" "default"))
                             (:module "array"
                              :components ((:file "reader")
                                           (:file "reader-optimize" :depends-on ("reader"))
                                           (:file "writer"))
                              :depends-on ("default"))
                             (:module "string"
                              :components ((:file "reader")
                                           (:file "writer"))
                              :depends-on ("integer"))
                             (:module "map"
                              :components ((:file "reader")
                                           (:file "writer")))
                             (:module "list"
                              :components ((:file "reader")
                                           (:file "writer"))))
                :depends-on ("package" "common" "reader" "place")))
  :in-order-to ((test-op (test-op #:binstruct/test))))

(defsystem binstruct/test
  :depends-on (#:binstruct #:parachute #:flexi-streams)
  :pathname "test/"
  :components ((:file "package")
               (:file "defbinio" :depends-on ("package"))
               (:file "bmp" :depends-on ("package")))
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:binstruct.test))))
