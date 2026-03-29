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
               (:file "interface" :depends-on ("package"))
               (:module "type"
                :components ((:file "default")
                             (:file "condition")
                             (:file "integer")
                             (:file "boolean" :depends-on ("integer"))
                             (:file "pointer" :depends-on ("integer" "default"))
                             (:file "sequence" :depends-on ("default"))
                             (:file "string")
                             (:file "custom")
                             (:file "list")
                             (:module "optimize"
                              :components ((:file "sequence"))
                              :depends-on ("sequence")))
                :depends-on ("package" "interface")))
  :in-order-to ((test-op (test-op #:binstruct/test))))

(defsystem binstruct/test
  :depends-on (#:binstruct #:parachute)
  :pathname "test/"
  :components ((:file "package")
               (:file "bmp" :depends-on ("package")))
  :perform (test-op (op c) (symbol-call '#:parachute '#:test (find-symbol (symbol-name '#:suite) '#:binstruct.test))))
