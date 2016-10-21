#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.protocol)

(defvar *id-counter* 0)

(defparameter *default-profile-lifetime* (* 60 60 24 365))
(defparameter *default-channel-lifetime* (* 60 60 24 30))

(defparameter *default-regular-channel-permissions*
  '((permissions (:registrant))
    (join T)
    (leave T)
    (kick (:registrant))
    (pull T)
    (message T)
    (users T)
    (channels T)))

(defparameter *default-anonymous-channel-permissions*
  '((permissions NIL)
    (join NIL)
    (leave T)
    (kick NIL)
    (pull T)
    (message T)
    (users NIL)
    (channels NIL)))

(defparameter *default-primary-channel-permissions*
  '((permissions (:registrant))
    (join T)
    (leave NIL)
    (kick (:registrant))
    (pull NIL)
    (message (:registrant))
    (users T)
    (channels T)))

(deftype wireable ()
  `(or real string cons symbol wire-object))

(defun username-p (name)
  (and (stringp name)
       (<= 1 (length name) 32)))

(deftype username ()
  `(satisfies username-p))

(defun channelname-p (name)
  (and (stringp name)
       (<= 1 (length name) 32)))

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
  (print-unreadable-object (object stream :type T)
    (format stream "~a" (maybe-sval object 'name))))

(define-protocol-class profile (named-object server-object)
  ((name :type username)
   (password :initarg :password :accessor password :type password)
   (lifetime :initarg :lifetime :accessor lifetime :type (integer 0)))
  (:default-initargs
   :lifetime *default-profile-lifetime*))

(define-protocol-class user (named-object server-object)
  ((connections :initarg :connections :accessor connections :type list)
   (channels :initarg :channels :accessor channels :type list))
  (:default-initargs
   :connections ()
   :channels ()))

(define-protocol-class connection (server-object)
  ((user :initarg :user :accessor user :type (or null user))
   (hostname :initarg :hostname :accessor hostname :type string)
   (port :initarg :port :accessor port :type (integer 0))))

(define-protocol-class channel (named-object server-object)
  ((name :type channelname)
   (permissions :initarg :permissions :accessor permissions :type list)
   (lifetime :initarg :lifetime :accessor lifetime :type (integer 0))
   (users :initarg :users :accessor users :type list))
  (:default-initargs
   :permissions ()
   :lifetime *default-channel-lifetime*
   :users ()))

;; Updates
(define-protocol-class wire-object ()
  ())

(define-protocol-class update (wire-object)
  ((id :initarg :id :accessor id :type id)
   (clock :initarg :clock :accessor clock :type integer)
   (from :initarg :from :accessor from :type username))
  (:default-initargs
   :id (next-id)
   :clock (get-universal-time)))

(defmethod print-object ((update update) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a" :from (maybe-sval update 'from)
                                 :id (maybe-sval update 'id))))

(define-protocol-class ping (update)
  ((clock :initarg :clock :accessor clock :type integer))
  (:default-initargs
   :clock (get-universal-time)))

(define-protocol-class pong (update)
  ((clock :initarg :clock :accessor clock :type integer))
  (:default-initargs
   :clock (get-universal-time)))

(define-protocol-class connect (update)
  ((password :initarg :password :accessor password :type (or null password))
   (version :initarg :version :accessor version :type string))
  (:default-initargs
   :password NIL
   :version (protocol-version)))

(define-protocol-class disconnect (update)
  ())

(define-protocol-class register (update)
  ((password :initarg :password :accessor password :type password)))

(define-protocol-class channel-update (update)
  ((channel :initarg :channel :accessor channel :type channelname)))

(define-protocol-class target-update (update)
  ((target :initarg :target :accessor target :type username)))

(define-protocol-class text-update (update)
  ((text :initarg :text :accessor text :type string)))

(define-protocol-class join (channel-update)
  ())

(define-protocol-class leave (channel-update)
  ())

(define-protocol-class create (channel-update)
  ())

(define-protocol-class kick (channel-update target-update)
  ())

(define-protocol-class pull (channel-update target-update)
  ())

(define-protocol-class permissions (channel-update)
  ((permissions :initarg :permissions :accessor permissions :type list)))

(define-protocol-class message (channel-update text-update)
  ())

(define-protocol-class users (channel-update)
  ((users :initarg :users :accessor users :type list)))

(define-protocol-class channels (update)
  ((channels :initarg :channels :accessor channels :type list))
  (:default-initargs :channels ()))

;; Errors
(define-protocol-class failure (text-update)
  ())

(defmethod print-object ((update failure) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a ~s ~s" :from (maybe-sval update 'from)
                                       :id (maybe-sval update 'id)
                                       :text (maybe-sval update 'text))))

(define-protocol-class malformed-update (failure)
  ())

(define-protocol-class connection-unstable (failure)
  ())

(define-protocol-class update-failure (failure)
  ((update-id :initarg :update-id :accessor update-id :type id)))

(define-protocol-class invalid-update (update-failure)
  ())

(define-protocol-class incompatible-version (update-failure)
  ((compatible-versions :initarg :compatible-versions :accessor compatible-versions :type cons)))

(define-protocol-class invalid-password (update-failure)
  ())

(define-protocol-class no-such-profile (update-failure)
  ())

(define-protocol-class username-taken (update-failure)
  ())

(define-protocol-class no-such-channel (update-failure)
  ())

(define-protocol-class already-in-channel (update-failure)
  ())

(define-protocol-class not-in-channel (update-failure)
  ())

(define-protocol-class channelname-taken (update-failure)
  ())

(define-protocol-class bad-name (update-failure)
  ())

(define-protocol-class insufficient-permissions (update-failure)
  ())

(define-protocol-class invalid-permissions (update-failure)
  ())

(define-protocol-class no-such-user (update-failure)
  ())

(define-protocol-class too-many-updates (update-failure)
  ())

;; Method calls that translate to updates on the wire
;; (defgeneric connect (hostname username &key password))
;; (defgeneric disconnect ())
;; (defgeneric register (password))
;; (defgeneric join (channel))
;; (defgeneric leave (channel))
;; (defgeneric create (&optional name))
;; (defgeneric kick (user channel))
;; (defgeneric pull (user channel))
;; (defgeneric permissions (channel))
;; (defgeneric (setf permissions) (perms channel))
;; (defgeneric update (target update))
;; (defgeneric send (text channel))
;; (defgeneric users (channel))
;; (defgeneric channels ())

;; The CLOCK field is omitted from each update in these illustrations,
;; but should be set to (get-universal-time) at the time the update
;; is constructed in a real application.
;; 
;;; Connection Handshake
;; you <> serv - TCP handshake
;; you  > serv - (connect :id 1 :from username :password pass :version client-proto-version)
;; On bad password:
;; you <  serv - (invalid-password :id 1 :from hostname)
;; you <> serv - TCP disconnect
;; On username taken:
;; you <  serv - (username-taken :id 1 :from hostname)
;; you <> serv - TCP disconnect
;; On success:
;; you <  serv - (connect :id 1 :version server-proto-version :from hostname)
;; all <  serv - (join    :id 3 :from username :channel hostname)
;; you <  serv - (users   :id 4 :from hostname :channel hostname :users (... ))
;;
;;; Manual Disconnection
;; you  > serv - (disconnect :id 5 :from username)
;; you <> serv - TCP disconnect
;;
;; If the user has no more connections left, a message is sent for each
;; channel that the user previously inhabited:
;; all <  serv - (leave :id 5 :from username :channel channel)
;;
;; Alternatively, if the server's channel is LEFT, all user connections
;; are immediately disconnected.
;;
;;; Generic update
;; Usually whenever the user sends an update, the response is then
;; broadcast to everyone in the channel, including the user themselves
;; if the update was successful.
;;
;; Manufactured updates must have a unique (to you) ID. This ID is
;; then re-used when distributing the update or constructing a response
;; to it such as for an error.
;;
;;; Some More Examples
;; Sending a message:
;; you  > serv - (message :id 7 :from username :channel channel :text "Hi.")
;; all <  serv - (message :id 7 :from username :channel channel :text "Hi.")
;;
;; Joining a channel:
;; you  > serv - (join :id 8 :from username :channel channel)
;; all <  serv - (join :id 8 :from username :channel channel)
;; you <  serv - (users :id 9 :from hostname :channel channel)
;;
;; Failing to set permissions:
;; you  > serv - (permissions :id 10 :from username :channel channel :permissions (... ))
;; you <  serv - (insufficient-permissions :id 10 :from hostname)
;;
;; Simulating privmsgs using an anonymous channel:
;; you  > serv - (create :id 11 :from username)
;; you <  serv - (join :id 11 :from username :channel unique-id)
;; you  > serv - (pull :id 12 :from username :channel unique-id :user otherguy)
;; guy <  serv - (join :id 13 :from otherguy :channel unique-id)
;; you <  serv - (join :id 14 :from otherguy :channel unique-id)
;; guy <  serv - (users :id 15 :from hostname :channel unique-id)
;; you  > serv - (message :id 16 :from username :channel unique-id :text "Yo.")
;; you <  serv - (message :id 16 :from username :channel unique-id :text "Yo.")
;; guy <  serv - (message :id 16 :from username :channel unique-id :text "Yo.")
;;
