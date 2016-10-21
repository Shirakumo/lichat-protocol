#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:lichat-protocol
  (:nicknames #:org.shirakumo.lichat.protocol)
  (:use #:cl)
  ;; conditions.lisp
  (:export
   #:protocol-condition
   #:printer-condition
   #:unprintable-object
   #:reader-condition
   #:incomplete-token
   #:unknown-symbol
   #:incompatible-value-type-for-slot)
  ;; printer.lisp
  (:export)
  ;; protocol.lisp
  (:export
   #:*default-profile-lifetime*
   #:*default-channel-lifetime*
   #:*id-counter*
   #:*default-regular-channel-permissions*
   #:*default-anonymous-channel-permissions*
   #:*default-primary-channel-permissions*
   #:wireable
   #:username-p
   #:username
   #:channelname-p
   #:channelname
   #:password-p
   #:password
   #:id-p
   #:id
   #:next-id
   #:protocol-version
   #:define-protocol-class
   #:server-object
   #:named-object
   #:name
   #:profile
   #:password
   #:lifetime
   #:user
   #:connections
   #:channels
   #:connection
   #:user
   #:hostname
   #:port
   #:channel
   #:permissions
   #:lifetime
   #:users
   #:wire-object
   #:ping
   #:pong
   #:update
   #:id
   #:clock
   #:from
   #:connect
   #:password
   #:version
   #:disconnect
   #:register
   #:channel-update
   #:channel
   #:target-update
   #:target
   #:text-update
   #:text
   #:join
   #:leave
   #:create
   #:kick
   #:pull
   #:permissions
   #:message
   #:users
   #:users
   #:channels
   #:channels
   #:failure
   #:malformed-update
   #:connection-unstable
   #:update-failure
   #:update-id
   #:invalid-update
   #:incompatible-version
   #:invalid-password
   #:no-such-profile
   #:username-taken
   #:no-such-channel
   #:already-in-channel
   #:not-in-channel
   #:channelname-taken
   #:bad-name
   #:insufficient-permissions
   #:invalid-permissions
   #:no-such-user
   #:too-many-updates)
  ;; reader.lisp
  (:export)
  ;; typed-slot-class.lisp
  (:export)
  ;; wire.lisp
  (:export
   #:to-wire
   #:from-wire))
