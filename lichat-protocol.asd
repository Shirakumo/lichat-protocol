(asdf:defsystem lichat-protocol
  :version "1.5"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "The independent protocol part of Lichat."
  :homepage "https://Shirakumo.github.io/lichat-protocol/"
  :bug-tracker "https://github.com/Shirakumo/lichat-protocol/issues"
  :source-control (:git "https://github.com/Shirakumo/lichat-protocol.git")
  :serial T
  :components ((:file "package")
               (:file "conditions")
               (:file "printer")
               (:file "reader")
               (:file "typed-slot-class")
               (:file "protocol")
               (:file "base-protocols")
               (:file "wire")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :closer-mop
               :trivial-package-local-nicknames
               #-sbcl :cl-unicode))
