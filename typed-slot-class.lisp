#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.protocol)

(defvar *unbound-value* (make-symbol "UNBOUND"))

(define-condition incompatible-value-type-for-slot (error)
  ((object :initarg :object)
   (slot :initarg :slot)
   (value :initarg :value)
   (type :initarg :type))
  (:report (lambda (c s) (format s "Setting ~s as value of slot ~s on ~s failed as it is not of type ~s."
                                 (slot-value c 'value) (slot-value c 'slot) (slot-value c 'object) (slot-value c 'type)))))

(defun check-compatible-slot-value (value object slot)
  (cond ((eq value *unbound-value*)
         (unless (eql T (c2mop:slot-definition-type slot))     
           (cerror "Unbind the slot anyway." 'incompatible-value-type-for-slot
                   :object object :slot (c2mop:slot-definition-name slot) :value *unbound-value* :type (c2mop:slot-definition-type slot))))
        ((not (typep value (c2mop:slot-definition-type slot)))
         (cerror "Write to the slot anyway." 'incompatible-value-type-for-slot
                 :object object :slot (c2mop:slot-definition-name slot) :value value :type (c2mop:slot-definition-type slot)))))

(defclass typed-slot (c2mop:standard-slot-definition)
  ())

(defmethod print-object ((slot typed-slot) stream)
  (print-unreadable-object (slot stream :type T :identity T)
    (format stream "~s ~s ~s" (c2mop:slot-definition-name slot) :type (c2mop:slot-definition-type slot))))

(defclass typed-direct-slot-definition (typed-slot c2mop:standard-direct-slot-definition)
  ())

(defclass typed-effective-slot-definition (typed-slot c2mop:standard-effective-slot-definition)
  ())

(defclass typed-slot-class (standard-class)
  ())

(defmethod c2mop:validate-superclass ((class typed-slot-class) (superclass t))
  NIL)

(defmethod c2mop:validate-superclass ((class standard-class) (superclass typed-slot-class))
  T)

(defmethod c2mop:validate-superclass ((class typed-slot-class) (superclass standard-class))
  T)

(defmethod c2mop:validate-superclass ((class typed-slot-class) (superclass typed-slot-class))
  T)

(defmethod c2mop:direct-slot-definition-class ((class typed-slot-class) &key)
  (find-class 'typed-direct-slot-definition))

(defmethod c2mop:effective-slot-definition-class ((class typed-slot-class) &key)
  (find-class 'typed-effective-slot-definition))

(defmethod (setf c2mop:slot-value-using-class) :before (value (class typed-slot-class) object (slot typed-slot))
  (check-compatible-slot-value value object slot))

(defmethod c2mop:slot-makunbound-using-class :before ((class typed-slot-class) object (slot typed-slot))
  (unless (slot-boundp object (c2mop:slot-definition-name slot))
    (check-compatible-slot-value *unbound-value* object slot)))

(defclass typed-object ()
  ()
  (:metaclass typed-slot-class))

(defmethod shared-initialize :after ((object typed-object) slot-names &key)
  (let ((slots (c2mop:class-slots (class-of object))))
    (flet ((process-slot (slot)
             (unless (slot-boundp object (c2mop:slot-definition-name slot))
               (check-compatible-slot-value *unbound-value* object slot))))
      (etypecase slot-names
        (list (loop for name in slot-names
                    for slot = (find name slots :key #'c2mop:slot-definition-name)
                    do (process-slot slot)))
        (T    (loop for slot in slots
                    do (process-slot slot)))))))

(defmacro define-typed-class (name direct-superclasses direct-slots &body options)
  `(defclass ,name (,@direct-superclasses typed-object)
     ,direct-slots
     ,@(unless (find :metaclass options :key #'first)
         `((:metaclass typed-slot-class)))
     ,@options))
