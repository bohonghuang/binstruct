(defsystem binstruct
  :version "0.1.0"
  :author "Bohong Huang <bohonghuang@qq.com>"
  :maintainer "Bohong Huang <bohonghuang@qq.com>"
  :license "Apache-2.0"
  :description "Declarative binary structure encoding and decoding for Common Lisp."
  :homepage "https://github.com/bohonghuang/binstruct"
  :bug-tracker "https://github.com/bohonghuang/binstruct/issues"
  :source-control (:git "https://github.com/bohonghuang/binstruct.git")
  :depends-on (#:alexandria #:parsonic)
  :pathname "src/"
  :components ((:file "package")
               (:file "interface" :depends-on ("package"))
               (:module "type"
                :components ((:file "condition")
                             (:file "integer")
                             (:file "pointer")
                             (:file "sequence")
                             (:file "string"))
                :depends-on ("package" "interface"))))
