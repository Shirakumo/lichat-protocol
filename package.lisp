#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(defpackage #:lichat-protocol
  (:nicknames #:org.shirakumo.lichat.protocol)
  (:local-nicknames
   (#:pln #:trivial-package-local-nicknames))
  (:use #:cl)
  (:shadow #:search #:block)
  ;; conditions.lisp
  (:export
   #:protocol-condition
   #:wire-condition
   #:printer-condition
   #:unprintable-object
   #:object
   #:null-in-symbol-designator
   #:symbol-designator
   #:reader-condition
   #:stray-null-found
   #:incomplete-token
   #:unknown-symbol
   #:symbol-designator
   #:read-limit-hit
   #:missing-update-argument
   #:update
   #:missing-id
   #:missing-clock
   #:unknown-wire-object
   #:malformed-wire-object
   #:incompatible-value-type-for-slot)
  ;; printer.lisp
  (:export
   #:print-sexpr)
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
   #:lifetime
   #:user
   #:connections
   #:channels
   #:connection
   #:user
   #:channel
   #:permissions
   #:grant
   #:update
   #:deny
   #:lifetime
   #:users
   #:object)
  ;; reader.lisp
  (:export
   #:whitespace-p
   #:skip-to-null
   #:read-sexpr)
  ;; typed-slot-class.lisp
  (:export)
  ;; wire.lisp
  (:export
   #:to-wire
   #:check-update-options
   #:from-wire
   #:from-wire*))

(defpackage #:org.shirakumo.lichat.protocol.packages
  (:use)
  (:local-nicknames
   (#:lichat #:org.shirakumo.lichat.protocol)
   ;; KLUDGE: Servers and clients currently alias these two packages.
   (#:shirakumo #:org.shirakumo.lichat.protocol)))
