#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.protocol)

(define-condition protocol-condition (condition)
  ())

(define-condition wire-condition (protocol-condition)
  ())

(define-condition printer-condition (wire-condition)
  ())

(define-condition unprintable-object (error printer-condition)
  ((object :initarg :object :reader object))
  (:report (lambda (c s) (format s "The object~%  ~s~%cannot be printed to the lichat wire format."
                                 (object c)))))

(define-condition reader-condition (wire-condition)
  ())

(define-condition incomplete-token (error reader-condition)
  ()
  (:report "Unable to read complete token-- end of stream reached prematurely."))

(define-condition unknown-symbol (error reader-condition)
  ((symbol-designator :initarg :symbol-designator :reader symbol-designator))
  (:report (lambda (c s) (format s "The symbol ~a::~a was found on the wire, but is not interned locally."
                                 (car (symbol-designator c)) (cdr (symbol-designator c))))))

(define-condition missing-update-argument (wire-condition)
  ((update :initarg :update :reader update))
  (:report (lambda (c s) (format s "The update did not include all necessary arguments:~%  ~s"
                                 (update c)))))

(define-condition missing-id (error missing-update-argument)
  ()
  (:report (lambda (c s) (format s "The update did not include an ID argument:~%  ~s"
                                 (update c)))))

(define-condition missing-clock (error missing-update-argument)
  ()
  (:report (lambda (c s) (format s "The update did not include a CLOCK argument:~%  ~s"
                                 (update c)))))

(define-condition unknown-wire-object (error wire-condition)
  ((update :initarg :update :reader update))
  (:report (lambda (c s) (format s "A wireable of type ~s was sent, but is not known.~%  ~s"
                                 (first (update c)) (update c)))))

(define-condition malformed-wire-object (error wire-condition)
  ((update :initarg :update :reader update))
  (:report (lambda (c s) (format s "A wireable was found but is not properly formed.~%  ~s"
                                 (update c)))))

(define-condition incompatible-value-type-for-slot (protocol-condition error)
  ((object :initarg :object :reader object)
   (slot :initarg :slot :reader slot)
   (value :initarg :value :reader value)
   (type :initarg :type :reader reqtype))
  (:report (lambda (c s) (format s "Setting ~s as value of slot ~s on ~s failed as it is not of type ~s."
                                 (slot-value c 'value) (slot-value c 'slot) (slot-value c 'object) (slot-value c 'type)))))
