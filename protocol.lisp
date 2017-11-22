#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.protocol)

(defvar *id-counter* (random (get-universal-time) (make-random-state T)))

(defparameter *default-profile-lifetime* (* 60 60 24 30))
(defparameter *default-channel-lifetime* (* 60 60 24 30))

(defparameter *default-regular-channel-permissions*
  '((permissions :registrant)
    (join T)
    (leave T)
    (kick :registrant)
    (pull T)
    (message T)
    (users T)
    (channels T)
    (backfill T)
    (data T)))

(defparameter *default-anonymous-channel-permissions*
  '((permissions)
    (join)
    (leave T)
    (kick :registrant)
    (pull T)
    (message T)
    (users)
    (channels)
    (backfill T)
    (data T)))

(defparameter *default-primary-channel-permissions*
  '((permissions :registrant)
    (create T)
    (join T)
    (leave)
    (kick :registrant)
    (pull)
    (message :registrant)
    (users T)
    (channels T)
    (backfill :registrant)
    (data :registrant)
    (emotes T)
    (emote :registrant)))

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
       (every #'valid-name-char-p name)))

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
   (password :initarg :password :accessor password :slot-type password)
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
(define-protocol-class wire-object ()
  ())

(define-protocol-class update (wire-object)
  ((id :initarg :id :accessor id :slot-type id)
   (clock :initarg :clock :accessor clock :slot-type integer)
   (from :initarg :from :accessor from :slot-type username))
  (:default-initargs
   :id (next-id)
   :clock (get-universal-time)))

(defmethod print-object ((update update) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a" :from (maybe-sval update 'from)
                                 :id (maybe-sval update 'id))))

(define-protocol-class ping (update)
  ())

(define-protocol-class pong (update)
  ())

(define-protocol-class connect (update)
  ((password :initarg :password :accessor password :slot-type (or null password))
   (version :initarg :version :accessor version :slot-type string)
   (extensions :initarg :extensions :accessor extensions :slot-type list))
  (:default-initargs
   :password NIL
   :version (protocol-version)
   :extensions ()))

(define-protocol-class disconnect (update)
  ())

(define-protocol-class register (update)
  ((password :initarg :password :accessor password :slot-type password)))

(define-protocol-class channel-update (update)
  ((channel :initarg :channel :accessor channel :slot-type channelname)))

(defmethod print-object ((update channel-update) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a ~s ~a"
            :from (maybe-sval update 'from)
            :channel (maybe-sval update 'channel)
            :id (maybe-sval update 'id))))

(define-protocol-class target-update (update)
  ((target :initarg :target :accessor target :slot-type username)))

(defmethod print-object ((update target-update) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a ~s ~a"
            :from (maybe-sval update 'from)
            :target (maybe-sval update 'target)
            :id (maybe-sval update 'id))))

(define-protocol-class text-update (update)
  ((text :initarg :text :accessor text :slot-type string)))

(defmethod print-object ((update text-update) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a ~s ~s" :from (maybe-sval update 'from)
                                       :id (maybe-sval update 'id)
                                       :text (maybe-sval update 'text))))

(define-protocol-class join (channel-update)
  ())

(define-protocol-class leave (channel-update)
  ())

(define-protocol-class create (channel-update)
  ((channel :initarg :channel :accessor channel :slot-type (or null channelname)))
  (:default-initargs :channel NIL))

(define-protocol-class kick (channel-update target-update)
  ())

(define-protocol-class pull (channel-update target-update)
  ())

(define-protocol-class permissions (channel-update)
  ((permissions :initarg :permissions :accessor permissions :slot-type list))
  (:default-initargs :permissions NIL))

(define-protocol-class message (channel-update text-update)
  ())

(define-protocol-class users (channel-update)
  ((users :initarg :users :accessor users :slot-type list))
  (:default-initargs :users ()))

(define-protocol-class channels (update)
  ((channels :initarg :channels :accessor channels :slot-type list))
  (:default-initargs :channels ()))

(define-protocol-class user-info (target-update)
  ((registered :initarg :registered :accessor registered :slot-type boolean)
   (connections :initarg :connections :accessor connections :slot-type (integer 1)))
  (:default-initargs :registered NIL :connections 1))

(define-protocol-class backfill (channel-update)
  ())

(define-protocol-class data (channel-update)
  ((content-type :initarg :content-type :accessor content-type :slot-type string)
   (filename :initarg :filename :accessor filename :slot-type (or null string))
   (payload :initarg :payload :accessor payload :slot-type string))
  (:default-initargs :file-name NIL))

(define-protocol-class emotes (update)
  ((names :initarg :names :accessor names :slot-type list))
  (:default-initargs :names NIL))

(define-protocol-class emote (update)
  ((content-type :initarg :content-type :accessor content-type :slot-type string)
   (name :initarg :name :accessor name :slot-type string)
   (payload :initarg :payload :accessor payload :slot-type string)))

;; Errors
(define-protocol-class failure (text-update)
  ())

(define-protocol-class malformed-update (failure)
  ()
  (:default-initargs :text "Update was malformed and could not be parsed."))

(define-protocol-class update-too-long (failure)
  ()
  (:default-initargs :text "The update was too long and has been dropped."))

(define-protocol-class connection-unstable (failure)
  ()
  (:default-initargs :text "The connection is unstable. You may be disconnected soon."))

(define-protocol-class too-many-connections (failure)
  ()
  (:default-initargs :text "There are too many connections for this user or on this server."))

(define-protocol-class update-failure (failure)
  ((update-id :initarg :update-id :accessor update-id :slot-type id)))

(defmethod print-object ((update update-failure) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a ~s ~a" :from (maybe-sval update 'from)
                                       :id (maybe-sval update 'id)
                                       :update-id (maybe-sval update 'update-id))))

(define-protocol-class invalid-update (update-failure)
  ()
  (:default-initargs :text "The update class is invalid."))

(define-protocol-class username-mismatch (update-failure)
  ()
  (:default-initargs :text "The FROM field did not match the known username of the connection."))

(define-protocol-class incompatible-version (update-failure)
  ((compatible-versions :initarg :compatible-versions :accessor compatible-versions :slot-type cons))
  (:default-initargs :text "The server and client versions are not compatible."))

(define-protocol-class invalid-password (update-failure)
  ()
  (:default-initargs :text "Invalid username or password."))

(define-protocol-class no-such-profile (update-failure)
  ()
  (:default-initargs :text "No such profile could be found."))

(define-protocol-class username-taken (update-failure)
  ()
  (:default-initargs :text "The requested username is already taken."))

(define-protocol-class no-such-channel (update-failure)
  ()
  (:default-initargs :text "No such channel exists on the server."))

(define-protocol-class already-in-channel (update-failure)
  ()
  (:default-initargs :text "The user is already in the channel."))

(define-protocol-class not-in-channel (update-failure)
  ()
  (:default-initargs :text "The user is not part of the channel."))

(define-protocol-class channelname-taken (update-failure)
  ()
  (:default-initargs :text "The requested channelname is already taken."))

(define-protocol-class bad-name (update-failure)
  ()
  (:default-initargs :text "The specified name is not a valid name."))

(define-protocol-class insufficient-permissions (update-failure)
  ()
  (:default-initargs :text "You do not have sufficient permissions to perform the requested action."))

(define-protocol-class invalid-permissions (update-failure)
  ()
  (:default-initargs :text "The permissions are malformed."))

(define-protocol-class no-such-user (update-failure)
  ()
  (:default-initargs :text "The requested user does not exist."))

(define-protocol-class too-many-updates (update-failure)
  ()
  (:default-initargs :text "You have been sending too many updates and have been throttled."))

(define-protocol-class bad-content-type (update-failure)
  ((allowed-content-types :initarg :allowed-content-types :accessor allowed-content-types :slot-type list))
  (:default-initargs :text "The supplied content type for the data update is not accepted by this server."
                     :allowed-content-types ()))
