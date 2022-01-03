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
(define-protocol-class wire-object ()
  ())

(define-protocol-class update (wire-object)
  ((id :initarg :id :accessor id :slot-type id)
   (clock :initarg :clock :accessor clock :slot-type integer)
   (from :initarg :from :accessor from :slot-type (or null username)))
  (:default-initargs
   :id (next-id)
   :clock (get-universal-time)
   :from NIL))

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
  ((channel :initarg :channel :accessor channel :slot-type channelname)
   (bridge :initarg :bridge :accessor bridge :slot-type (or null username)))
  (:default-initargs
   :bridge NIL))

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

(define-protocol-class grant (channel-update target-update)
  ((update :initarg :update :accessor update :slot-type symbol)))

(define-protocol-class deny (channel-update target-update)
  ((update :initarg :update :accessor update :slot-type symbol)))

(define-protocol-class message (channel-update text-update)
  ((link :initarg :link :accessor link :slot-type (or null string)))
  (:default-initargs :link NIL))

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

(define-protocol-class capabilities (channel-update)
  ((permitted :initarg :permitted :accessor permitted :slot-type list))
  (:default-initargs :permitted ()))

(define-protocol-class server-info (target-update)
  ((attributes :initarg :attributes :accessor attributes :slot-type list)
   (connections :initarg :connections :accessor connections :slot-type list))
  (:default-initargs :attributes () :connections ()))

(define-protocol-class backfill (channel-update)
  ())

(define-protocol-class data (channel-update)
  ((content-type :initarg :content-type :accessor content-type :slot-type string)
   (filename :initarg :filename :accessor filename :slot-type (or null string))
   (payload :initarg :payload :accessor payload :slot-type string))
  (:default-initargs :filename NIL))

(define-protocol-class emotes (channel-update)
  ((names :initarg :names :accessor names :slot-type list))
  (:default-initargs :names NIL))

(define-protocol-class emote (channel-update)
  ((content-type :initarg :content-type :accessor content-type :slot-type string)
   (name :initarg :name :accessor name :slot-type string)
   (payload :initarg :payload :accessor payload :slot-type string)))

(define-protocol-class edit (message)
  ())

(define-protocol-class channel-info (channel-update)
  ((keys :initarg :keys :accessor keys :slot-type (or list T)))
  (:default-initargs :keys T))

(define-protocol-class set-channel-info (channel-update)
  ((key :initarg :key :accessor key :slot-type symbol)
   (text :initarg :text :accessor text :slot-type string)))

(define-protocol-class kill (target-update)
  ())

(define-protocol-class destroy (channel-update)
  ())

(define-protocol-class ban (target-update)
  ())

(define-protocol-class unban (target-update)
  ())

(define-protocol-class banned ()
  ((target :initarg :target :accessor target :slot-type list)))

(define-protocol-class ip-ban ()
  ((ip :initarg :ip :accessor ip :slot-type string)
   (mask :initarg :mask :accessor mask :slot-type string)))

(define-protocol-class ip-banned ()
  ((target :initarg :target :accessor target :slot-type list)))

(define-protocol-class ip-unban ()
  ((ip :initarg :ip :accessor ip :slot-type string)
   (mask :initarg :mask :accessor mask :slot-type string)))

(define-protocol-class pause (channel-update)
  ((by :initarg :by :accessor by :slot-type (integer 0))))

(define-protocol-class quiet (channel-update target-update)
  ())

(define-protocol-class unquiet (channel-update target-update)
  ())

(define-protocol-class quieted (channel-update)
  ((target :initarg :target :accessor target :slot-type list)))

(define-protocol-class typing (channel-update)
  ())

(define-protocol-class react (channel-update)
  ((emote :initarg :emote :accessor emote :slot-type string)
   (target :initarg :target :accessor target :slot-type username)
   (update-id :initarg :update-id :accessor update-id :slot-type id)))

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

(define-protocol-class already-connected (update-failure)
  ()
  (:default-initargs :text "You have already connected to an account."))

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

(define-protocol-class registration-rejected (update-failure)
  ()
  (:default-initargs :text "Your profile registration was rejected."))

(define-protocol-class already-in-channel (update-failure)
  ()
  (:default-initargs :text "The user is already in the channel."))

(define-protocol-class not-in-channel (update-failure)
  ()
  (:default-initargs :text "The user is not part of the channel."))

(define-protocol-class channelname-taken (update-failure)
  ()
  (:default-initargs :text "The requested channelname is already taken."))

(define-protocol-class too-many-channels (update-failure)
  ()
  (:default-initargs :text "The user already inhabits the maximum number of channels."))

(define-protocol-class bad-name (update-failure)
  ()
  (:default-initargs :text "The specified name is not a valid name."))

(define-protocol-class insufficient-permissions (update-failure)
  ()
  (:default-initargs :text "You do not have sufficient permissions to perform the requested action."))

(define-protocol-class invalid-permissions (update-failure)
  ((rule :initarg :rule :accessor rule :slot-type list))
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

(define-protocol-class no-such-parent-channel (udpate-failure)
  ()
  (:default-initargs :text "The channel you are trying to create a child channel under does not exist."))

(define-protocol-class no-such-channel-info (update-failure)
  ((key :initarg :key :accessor key :slot-type symbol))
  (:default-initargs :text "The requested channel info key does not exist."))

(define-protocol-class malformed-channel-info (update-failure)
  ()
  (:default-initargs :text "The specified info was not of the correct format for the key."))

(define-protocol-class clock-skewed (update-failure)
  ()
  (:default-initargs :text "Your clock is out of sync. You should synchronise with a time server."))

(define-protocol-class bad-ip-format (update-failure)
  ()
  (:default-initargs :text "The IP address was not in the correct format."))
