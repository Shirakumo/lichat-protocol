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
                             for name = (first (c2mop:slot-definition-initargs slot))
                             when name collect name
                             when name collect (slot-value wireable (c2mop:slot-definition-name slot))))
                  stream))
    (wireable
     (print-sexpr wireable stream)))
  ;; (terpri stream)
  (force-output stream))

(defun from-wire (stream)
  (let ((sexpr (read-sexpr stream)))
    (typecase sexpr
      (cons
       (unless (typep (first sexpr) 'symbol)
         (error 'malformed-wire-object :update sexpr))
       (let ((class (find-class (first sexpr) NIL)))
         (unless class (error 'unknown-wire-object :update sexpr))
         (cond ((c2mop:subclassp class (find-class 'update))
                (let (id-found clock-found)
                  (loop for (key val) on (rest sexpr) by #'cddr
                        do (unless (typep (first sexpr) 'keyword)
                             (error 'malformed-wire-object :update sexpr))
                           (case key
                             (:id (setf id-found T))
                             (:clock (setf clock-found T))))
                  (unless id-found
                    (error 'missing-id :update sexpr))
                  (unless clock-found
                    (error 'missing-clock :update sexpr)))
                (apply #'make-instance sexpr))
               ((c2mop:subclassp class (find-class 'wire-object))
                (apply #'make-instance sexpr))
               (T
                (error 'unknown-wire-object :update sexpr)))))
      (T
       sexpr))))
