#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem lichat-protocol
  :version "1.4"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
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
               (:file "wire")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :closer-mop
               #-sbcl :cl-unicode))
