#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.protocol)

(defvar *default-profile-lifetime* (* 60 60 24 365))
(defvar *default-channel-lifetime* (* 60 60 24 30))
(defvar *id-counter* 0)

(deftype wireable ()
  `(or real string cons symbol wire-object))

(defun check-username (name)
  (check-type name string)
  (assert (<= 1 (length name) 32)))

(defun check-channelname (name)
  (check-type name string)
  (assert (<= 1 (length name) 32)))

(defun check-password (pass)
  (check-type pass string)
  (assert (<= 6 (length pass))))

(defun check-id (id)
  (check-type id (not null)))

(defun check-permissions (perms)
  (check-type perms list))

(defun next-id ()
  (incf *id-counter*))

(defun protocol-version ()
  (asdf:component-version
   (asdf:find-system :lichat-protocol)))

;; Server-side objects
(defclass server-object ()
  ())

(defclass named-object ()
  ((name :initarg :name :accessor name)))

(defmethod print-object ((object named-object) stream)
  (print-unreadable-object (object stream :type T)
    (format stream "~a" (name object))))

(defclass profile (named-object server-object)
  ((password :initarg :password :accessor password)
   (lifetime :initarg :lifetime :accessor lifetime))
  (:default-initargs
   :lifetime *default-profile-lifetime*))

(defmethod initialize-instance :before ((profile profile) &key name password lifetime)
  (check-password password)
  (check-username name)
  (check-type lifetime (integer 0)))

(defclass user (named-object server-object)
  ((connections :initarg :connections :accessor connections)
   (channels :initarg :channels :accessor channels))
  (:default-initargs
   :connections ()
   :channels ()))

(defmethod initialize-instance :before ((user user) &key name connections channels)
  (check-username name)
  (check-type connections list)
  (check-type channels list))

(defclass connection (server-object)
  ((user :initarg :user :accessor user)
   (hostname :initarg :hostname :accessor hostname)
   (port :initarg :port :accessor port)))

(defmethod initialize-instance :before ((connection connection) &key user hostname port)
  (check-type user user)
  (check-type hostname string)
  (check-type port integer))

(defclass channel (named-object server-object)
  ((permissions :initarg :permissions :accessor permissions)
   (lifetime :initarg :lifetime :accessor lifetime)
   (users :initarg :users :accessor users)))

(defmethod initialize-instance :before ((channel channel) &key name permissions lifetime users)
  (check-channelname name)
  (check-permissions permissions)
  (check-type lifetime (integer 0))
  (check-type users list))

;; Updates
(defclass wire-object ()
  ())

(defclass update (wire-object)
  ((id :initarg :id :accessor id)
   (clock :initarg :clock :accessor clock)
   (from :initarg :from :accessor from))
  (:default-initargs
   :id (next-id)
   :clock (get-universal-time)))

(defmethod initialize-instance :before ((update update) &key id clock from)
  (check-id id)
  (check-username from)
  (check-type clock integer))

(defmethod print-object ((update update) stream)
  (print-unreadable-object (update stream :type T)
    (format stream "~s ~a ~s ~a" :from (from update) :id (id update))))

(defclass connect (update)
  ((password :initarg :password :accessor password)
   (version :initarg :version :accessor version))
  (:default-initargs
   :version (protocol-version)))

(defmethod initialize-instance :before ((update connect) &key password version)
  (when password (check-password password))
  (check-type version string))

(defclass disconnect (update)
  ())

(defclass register (update)
  ((password :initarg :password :accessor password)))

(defmethod initialize-instance :before ((update register) &key password)
  (check-password password))

(defclass channel-update (update)
  ((channel :initarg :channel :accessor channel)))

(defmethod initialize-instance :before ((update channel-update) &key channel)
  (check-channelname channel))

(defclass target-update (update)
  ((target :initarg :target :accessor target)))

(defmethod initialize-instance :before ((update target-update) &key target)
  (check-username target))

(defclass text-update (update)
  ((text :initarg :text :accessor text)))

(defmethod initialize-instance :before ((update text-update) &key text)
  (check-type text string))

(defclass join (channel-update)
  ())

(defclass leave (channel-update)
  ())

(defclass create (channel-update)
  ())

(defclass kick (channel-update target-update)
  ())

(defclass pull (channel-update target-update)
  ())

(defclass permissions (channel-update)
  ((permissions :initarg :permissions :accessor permissions)))

(defmethod initialize-instance :before ((update permissions) &key permissions)
  (check-permissions permissions))

(defclass message (channel-update text-update)
  ())

(defclass users (channel-update)
  ((users :initarg :users :accessor users)))

(defmethod initialize-instance :before ((update users) &key users)
  (check-type users list))

(defclass channels ()
  ((channels :initarg :channels :accessor channels)))

(defmethod initialize-instance :before ((update channels) &key channels)
  (check-type channels list))

;; Errors
(defclass failure (text-update)
  ())

(defclass malformed-update (failure)
  ())

(defclass connection-unstable (failure)
  ())

(defclass update-failure (failure)
  ((update-id :initarg :update-id :accessor update-id)))

(defmethod initialize-instance :before ((update update-failure) &key update-id)
  (check-id update-id))

(defclass invalid-update (update-failure)
  ())

(defclass invalid-password (update-failure)
  ())

(defclass no-such-profile (update-failure)
  ())

(defclass username-taken (update-failure)
  ())

(defclass no-such-channel (update-failure)
  ())

(defclass already-in-channel (update-failure)
  ())

(defclass not-in-channel (update-failure)
  ())

(defclass channelname-taken (update-failure)
  ())

(defclass insufficient-permissions (update-failure)
  ())

(defclass invalid-permissions (update-failure)
  ())

(defclass no-such-user (update-failure)
  ())

(defclass too-many-updates (update-failure)
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
;;; Channels and Permissions:
;; On creation, anonymous channels get the following default permissions:
;;  - JOIN             NIL  - Does not allow JOINing the channel, must be PULLed.
;;  - CHANNELS         NIL  - Prevents seeing the channel in the CHANNELS listing.
;;  - TIMEOUT            0  - Makes the channel be deleted as soon as there are
;;                            no more users in it.
;; Other channels get the following default permissions:
;;  - PERMISSIONS     user  - Allows the creator to change permissions.
;;  - JOIN               T  - Allows anyone to join.
;;  - KICK            user  - Allows the creator to kick people.
;;  - PULL               T  - Allows anyone to invite.
;;  - MESSAGE            T  - Allows anyone to send messages.
;;  - CHANNELS           T  - Allows seeing the channel in the CHANNELs listing.
;;
;; The server channel gets the following default permissions:
;;  - PERMISSIONS hostname  - Only the server can change permissions.
;;  - JOIN               T  - Anyone can enter.
;;  - KICK        hostname  - Only the server can kick.
;;  - PULL             NIL  - Doesn't make any sense anyway.
;;  - MESSAGE     hostname  - Allows only the server itself to send messages.
;;  - CHANNELS           T  - Allows anyone to see the server channel.
;;
;; Implicit permissions that cannot be changed by a user:
;;  - LEAVE              T  - Allows anyone to leave.
;;  - USERS              T  - Allows anyone to see which users are in.
;;  - TIMEOUT           32  - Number of days of inactivity until deletion.
