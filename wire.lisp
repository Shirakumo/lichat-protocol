#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.protocol)

(defun to-wire (wireable stream)
  (etypecase wireable
    (wire-object
     (print-sexpr `(,(class-name (class-of wireable))
                     ,@(loop for slot in (c2mop:class-slots (class-of wireable))
                             collect (first (c2mop:slot-definition-initargs slot))
                             collect (slot-value wireable (c2mop:slot-definition-name slot))))
                  stream))
    (wireable
     (print-sexpr wirable stream))))

(defun from-wire (stream)
  (let ((sexpr (read-sexpr stream)))
    (typecase sexpr
      (cons
       (check-type (first sexpr) symbol)
       (loop for (key val) on (rest sexpr) by #'cddr
             do (check-type key keyword))
       (apply #'make-instance sexpr))
      (T
       sexpr))))
