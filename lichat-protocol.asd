#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem lichat-protocol
  :version "1.2"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "The independent protocol part of Lichat."
  :homepage "https://github.com/Shirakumo/lichat-protocol"
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
