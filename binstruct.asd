(defsystem binstruct
  :version "0.1.0"
  :author "Bohong Huang <bohonghuang@qq.com>"
  :maintainer "Bohong Huang <bohonghuang@qq.com>"
  :license "Apache-2.0"
  :description "Declarative binary structure encoding and decoding for Common Lisp."
  :homepage "https://github.com/bohonghuang/binstruct"
  :bug-tracker "https://github.com/bohonghuang/binstruct/issues"
  :source-control (:git "https://github.com/bohonghuang/binstruct.git")
  :depends-on (#:alexandria #:parsonic #:parsonic.stream)
  :pathname "src/"
  :components ((:file "package")
               (:file "common" :depends-on ("package"))
               (:file "place" :depends-on ("package"))
               (:file "reader" :depends-on ("package" "common" "place"))
               (:file "macro" :depends-on ("package" "common" "reader" "place"))
               (:module "type"
                :components ((:file "default")
                             (:module "condition"
                              :components ((:file "reader")))
                             (:module "integer"
                              :components ((:file "reader")))
                             (:module "boolean"
                              :components ((:file "reader"))
                              :depends-on ("integer"))
                             (:module "pointer"
                              :components ((:file "reader"))
                              :depends-on ("integer" "default"))
                             (:module "array"
                              :components ((:file "reader")
                                           (:file "reader-optimize" :depends-on ("reader")))
                              :depends-on ("default"))
                             (:module "string"
                              :components ((:file "reader"))
                              :depends-on ("integer"))
                             (:module "map"
                              :components ((:file "reader")))
                             (:module "list"
                              :components ((:file "reader"))))
                :depends-on ("package" "common" "reader" "place")))
  :in-order-to ((test-op (test-op #:binstruct/test))))

(defsystem binstruct/test
  :depends-on (#:binstruct #:parachute)
  :pathname "test/"
  :components ((:file "package")
               (:file "bmp" :depends-on ("package")))
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:binstruct.test))))
