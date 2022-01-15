#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.protocol)

(defvar *protocol-extensions* ())

(defvar *id-counter* (random (get-universal-time) (make-random-state T)))

(defparameter *default-profile-lifetime* (* 60 60 24 30))
(defparameter *default-channel-lifetime* (* 60 60 24 30))

(defparameter *default-regular-channel-permissions*
  '((permissions (+ :registrant))
    (join T)
    (leave T)
    (kick (+ :registrant))
    (pull T)
    (message T)
    (users T)
    (channels T)
    (backfill T)
    (data T)
    (edit T)
    (channel-info T)
    (set-channel-info (+ :registrant))))

(defparameter *default-anonymous-channel-permissions*
  '((permissions)
    (join)
    (leave T)
    (kick (+ :registrant))
    (pull T)
    (message T)
    (users)
    (channels)
    (backfill T)
    (data T)
    (edit T)
    (channel-info)
    (set-channel-info)))

(defparameter *default-primary-channel-permissions*
  '((permissions (+ :registrant))
    (create T)
    (join T)
    (leave)
    (kick (+ :registrant))
    (pull)
    (message (+ :registrant))
    (users T)
    (channels T)
    (backfill (+ :registrant))
    (data (+ :registrant))
    (emotes T)
    (emote (+ :registrant))
    (edit)
    (channel-info T)
    (set-channel-info (+ :registrant))))

(deftype wireable ()
  `(or real string cons symbol wire-object))

(defun valid-name-char-p (c)
  (or (char= #\Space c)
      (let ((category (char #+sbcl (symbol-name (sb-unicode:general-category c))
                            #-sbcl (cl-unicode:general-category c) 0)))
        (not (or (char= category #\Z)
                 (char= category #\C))))))

(defun username-p (name)
  (and (stringp name)
       (<= 1 (length name) 32)
       (every #'valid-name-char-p name)
       (char/= #\Space (char name 0))
       (char/= #\Space (char name (1- (length name))))))

(deftype username ()
  `(satisfies username-p))

(defun channelname-p (name)
  (username-p name))

(deftype channelname ()
  `(satisfies channelname-p))

(defun password-p (pass)
  (and (stringp pass)
       (<= 6 (length pass))))

(deftype password ()
  `(satisfies password-p))

(defun id-p (id)
  (not (null id)))

(deftype id ()
  `(satisfies id-p))

(defun next-id ()
  (incf *id-counter*))

(defun protocol-version ()
  (asdf:component-version
   (asdf:find-system :lichat-protocol)))

(defmacro define-protocol-class (name direct-superclasses direct-slots &rest options)
  `(define-typed-class ,name ,direct-superclasses ,direct-slots ,@options))

(defun maybe-sval (object slot)
  (if (slot-boundp object slot)
      (slot-value object slot)
      *unbound-value*))

;; Server-side objects
(define-protocol-class server-object ()
  ())

(define-protocol-class named-object ()
  ((name :initarg :name :accessor name)))

(defmethod print-object ((object named-object) stream)
  (print-unreadable-object (object stream :type T :identity T)
    (format stream "~a" (maybe-sval object 'name))))

(define-protocol-class profile (named-object server-object)
  ((name :slot-type username)
   (lifetime :initarg :lifetime :accessor lifetime :slot-type (integer 0)))
  (:default-initargs
   :lifetime *default-profile-lifetime*))

(define-protocol-class user (named-object server-object)
  ((name :slot-type username)
   (connections :initarg :connections :accessor connections :slot-type list)
   (channels :initarg :channels :accessor channels :slot-type list))
  (:default-initargs
   :connections ()
   :channels ()))

(define-protocol-class connection (server-object)
  ((user :initarg :user :accessor user :slot-type (or null user)))
  (:default-initargs
   :user NIL))

(defmethod print-object ((connection connection) stream)
  (print-unreadable-object (connection stream :type T)
    (if (user connection)
        (format stream "~a/~d"
                (name (user connection))
                (position connection (connections (user connection))))
        (format stream "[unassociated]"))))

(define-protocol-class channel (named-object server-object)
  ((name :slot-type channelname)
   (permissions :initarg :permissions :accessor permissions :slot-type list)
   (lifetime :initarg :lifetime :accessor lifetime :slot-type (integer 0))
   (users :initarg :users :accessor users :slot-type list))
  (:default-initargs
   :permissions ()
   :lifetime *default-channel-lifetime*
   :users ()))

;; Updates
(defclass object () ())

(defmacro define-package (name)
  (let ((fqdn (format NIL "~:@(org.shirakumo.lichat.protocol.packages.~a~)" name))
        (local (string-upcase name)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (unless (find ,local (pln:package-local-nicknames '#:org.shirakumo.lichat.protocol.packages)
                     :key #'car :test #'string-equal)
         (make-package ,fqdn :use ())
         (pln:add-package-local-nickname
          ,local ,fqdn '#:org.shirakumo.lichat.protocol.packages)))))

(defmacro define-extension (name &body extensions)
  `(progn
     (pushnew ,(string-downcase name) *protocol-extensions* :test #'string-equal)
     ,@extensions))

(defun field-definition-to-slot-definition (field)
  (destructuring-bind (name type &rest args) field
    (let ((slot-name (if (eql (find-package "KEYWORD") (symbol-package name))
                         (intern (string-upcase name) '#:org.shirakumo.lichat.protocol)
                         name)))
      (list* slot-name :initarg name
                       :accessor slot-name
                       :slot-type type
                       (unless (find :optional args)
                         `(:initform (error ,(format NIL "~s required." name))))))))

(defmacro define-object (name superclasses &body fields)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (define-protocol-class ,name ,(or superclasses '(object))
       ,(mapcar #'field-definition-to-slot-definition fields))
     (export ',name ',(package-name (symbol-package name)))
     ,@(loop for field in fields
             for accessor = (getf (rest (field-definition-to-slot-definition field)) :accessor)
             collect `(export ',accessor ',(package-name (symbol-package accessor))))))

(defmacro define-object-extension (name superclasses &body fields)
  (let ((class (find-class name)))
    `(progn
       (define-protocol-class ,name ,(union (remove 'typed-object (mapcar #'class-name (c2mop:class-direct-superclasses class)))
                                            superclasses)
         (,@(loop for slot in (c2mop:class-direct-slots class)
                  do (setf fields (remove (first (c2mop:slot-definition-initargs slot))
                                          fields :key #'first))
                  collect (list* (c2mop:slot-definition-name slot)
                                 :initarg (first (c2mop:slot-definition-initargs slot))
                                 :accessor (first (c2mop:slot-definition-readers slot))
                                 :slot-type (slot-type slot)
                                 (when (c2mop:slot-definition-initform slot)
                                   `(:initform ,(c2mop:slot-definition-initform slot)))))
          ,@(mapcar #'field-definition-to-slot-definition fields)))
       ,@(loop for field in fields
               for accessor = (getf (rest (field-definition-to-slot-definition field)) :accessor)
               collect `(export ',accessor ',(package-name (symbol-package accessor)))))))

(defun read-protocol-file (file)
  (with-open-file (stream (merge-pathnames file #.(or *compile-file-pathname* *load-pathname*)) :direction :input)
    (let ((*package* (find-package '#:org.shirakumo.lichat.protocol.packages))
          (*read-import* T)
          (lines ()))
      (handler-case
          (loop for line = (progn (skip-whitespace stream)
                                  (read-sexpr stream))
                do (push line lines)
                   (when (eql 'define-package (first line))
                     (eval line)))
        (end-of-file ()
          (nreverse lines))))))

(defmacro define-from-protocol-file (file)
  `(progn ,@(read-protocol-file file)))
