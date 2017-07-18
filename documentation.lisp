#|
 This file is a part of lichat
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.lichat.protocol)

;; conditions.lisp
(docs:define-docs
  (type protocol-condition
    "Superclass for all conditions relating to the protocol.

See WIRE-CONDITION
See INCOMPATIBLE-VALUE-TYPE-FOR-SLOT")

  (type wire-condition
    "Superclass for all conditions relating to the wire format.

See PRINTER-CONDITION
See READER-CONDITION
See MISSING-UPDATE-ARGUMENT
See UNKNOWN-WIRE-OBJECT
See MALFORMED-WIRE-OBJECT")

  (type printer-condition
    "Superclass for all conditions relating to printing to the wire.

See UNPRINTABLE-OBJECT")

  (type unprintable-object
    "Condition signalled when an unprintable object is attempted to be put onto the wire.

See OBJECT")

  (function object
    "The object related to the condition.

See UNPRINTABLE-OBJECT
See INCOMPATIBLE-VALUE-TYPE-FOR-SLOT")

  (type null-in-symbol-designator
    "Condition signalled when a symbol is attempted to be put to the wire whose designator contains NULL characters.

See SYMBOL-DESIGNATOR")

  (function symbol-designator
    "A symbol designator.

Returns a CONS of two strings for the package- and symbol-name respectively.

See NULL-IN-SYMBOL-DESIGNATOR
See UNKNOWN-SYMBOL")

  (type reader-condition
    "Superclass for all conditions relating to reading from the wire.

See INCOMPLETE-TOKEN
See UNKNOWN-SYMBOL")

  (type incomplete-token
    "Condition signalled when a token is not complete on the wire and thus can't be read fully.")

  (type unknown-symbol
    "Condition signalled when an unknown symbol is found on the wire.

See SYMBOL-DESIGNATOR")

  (type missing-update-argument
    "Superclass for all conditions relating to missing required arguments in updates.

See MISSING-ID
See MISSING-CLOCK
See UPDATE")

  (function update
    "The update object that relates to the condition.

See MISSING-UPDATE-ARGUMENT
See UNKNOWN-WIRE-OBJECT")

  (type missing-id
    "Condition signalled when the ID field is missing from an update.")

  (type missing-clock
    "Condition signalled when the CLOCK field is missing from an update.")

  (type unknown-wire-object
    "Condition signalled when an object is found on the wire that has an unknown type.

See UPDATE")

  (type malformed-wire-object
    "Condition signalled when an object is found on the wire but is malformed and can't be parsed.

See UPDATE")

  (type incompatible-value-type-for-slot
    "Condition signalled when an incompatible type is attempted to be set on a slot.

See OBJECT
See SLOT
See VALUE
See REQTYPE")

  (function slot
    "The slot that was attempted to be set.

See INCOMPATIBLE-VALUE-TYPE-FOR-SLOT")

  (function value
    "The value that was attempted to be set.

See INCOMPATIBLE-VALUE-TYPE-FOR-SLOT")

  (function reqtype
    "The required type for the slot that was attempted to be set.

See INCOMPATIBLE-VALUE-TYPE-FOR-SLOT"))

;; printer.lisp
(docs:define-docs
  (function print-sexpr-list
    "Print an s-expression list to the stream.

This only handles proper lists. List items are printed recursively.

See PRINT-SEXPR")

  (function print-sexpr-string
    "Print an s-expression string to the stream.

Prints all characters verbatim, with the exception of \", which is escaped with a backslash.")

  (function print-sexpr-number
    "Print an s-expression number to the stream.

Only integers and floats can be printed, which are printed according to the
CLHS rules on printing numbers.")

  (function print-sexpr-token
    "Print an s-expression token to the stream.

All characters in the token string are printed verbatim, with the exception of
 \"():0123456789. #
which are escaped by a backslash.")

  (function print-sexpr-symbol
    "Print an s-expression symbol to the stream.

Symbols are printed as follows:
  KEYWORD:FOO          => :FOO
  LICHAT-PROTOCOL:FOO  => FOO
  #:FOO                => #:FOO
  SOMEWHERE:FOO        => SOMEWHERE:FOO

See PRINT-SEXPR-TOKEN")

  (function print-sexpr
    "Print an s-expression to the stream.

Only the following types are allowed:
 LIST STRING REAL SYMBOL
Any other type will signal an error of type UNPRINTABLE-OBJECT.

See PRINT-SEXPR-LIST
See PRINT-SEXPR-STRING
See PRINT-SEXPR-NUMBER
See PRINT-SEXPR-SYMBOL
See UNPRINTABLE-OBJECT"))

;; protocol.lisp
(docs:define-docs
  (variable *id-counter*
    "Counter variables for update IDs.

Starts at a random integer between 0 and the current universal-time.
This is done to make the clashing of IDs less likely to occur between
users of the protocol library.

See NEXT-ID")

  (variable *default-profile-lifetime*
    "The default lifetime for a registered profile in seconds.

Should equal to one non-leap year.")

  (variable *default-channel-lifetime*
    "The default lifetime for a new channel in seconds.

Should equal to one standard month (30 days).")

  (variable *default-regular-channel-permissions*
    "Default permissions for non-primary, non-anonymous channels.")

  (variable *default-anonymous-channel-permissions*
    "Default permissions for anonymous channels.")

  (variable *default-primary-channel-permissions*
    "Default permissions for primary/server channels.")

  (type wireable
    "Type for all objects that are permitted to appear on the wire.

Should be the union of REAL STRING CONS SYMBOL WIRE-OBJECT")

  (function username-p
    "Returns true if the given name is a valid name for users.

That is to say, the name must be a string in [1,32] of length.")

  (type username
    "Type that is satisfied for all username strings.

See USERNAME-P")

  (function channelname-p
    "Returns true if the given name is a valid name for channels.

That is to say, the name must be a string in [1,32] of length.")

  (type channelname
    "Type that is satisfied for all channelname strings.

See CHANNELNAME-P")

  (function password-p
    "Returns true if the given object is a string of at least six characters.")

  (type password
    "Type that is satisfied for all password strings.

See PASSWORD-P")

  (function id-p
    "Returns true if the given object is a valid ID.")

  (type id
    "Type that is satisfied for all ID objects.

See ID-P")

  (function next-id
    "Returns a fresh ID.

See *ID-COUNTER*")

  (function protocol-version
    "Returns the version string for the protocol.")

  (function define-protocol-class
    "Defines a new protocol class.

See DEFINE-TYPED-CLASS")

  (function maybe-sval
    "Returns the slot's value if it is bound, or *UNBOUND-VALUE* if it is not.")

  (type server-object
    "Superclass for all objects that exist on the server-side.")

  (type named-object
    "Superclass for all objects that are named.

See NAME")

  (function name
    "Accessor to the object's name string.

See NAMED-OBJECT")

  (type profile
    "Class to represent user profiles. Only registered users have a profile.

See NAMED-OBJECT
See SERVER-OBJECT
See NAME
See PASSWORD
See LIFETIME
See *DEFAULT-PROFILE-LIFETIME*")

  (function password
    "Accessor to the password of the object.

See PROFILE
See REGISTER
See CONNECT")

  (function lifetime
    "Accessor to the lifetime of the object.

The object should stay alive for at least this many
seconds after it has ceased to be used.

See PROFILE
See CHANNEL")

  (type user
    "Class to represent a user. Anything that can communicate with the server must have a user obuject.


See NAMED-OBJECT
See SERVER-OBJECT
See CONNECTIONS
See CHANNELS")

  (function connections
    "Accessor to the list of connections of the user or count of connections of the user-info.

See USER
See USER-INFO")

  (function channels
    "Accessor to the list of channels of the object.

See USER
See CHANNELS")

  (type connection
    "Class to represent a connection of a user.

See SERVER-OBJECT
See USER")

  (function user
    "Accessor to the user tied to the connection.

See CONNECTION
See USER")

  (type channel
    "Class to represent a channel.

See NAMED-OBJECT
See SERVER-OBJECT
See PERMISSIONS
See LIFETIME
See USERS
See *DEFAULT-CHANNEL-LIFETIME*")

  (function permissions
    "Accessor to the permissions list of the object.

See CHANNEL
See PERMISSIONS")

  (function users
    "Accessor to the list of users of the object.

See CHANNEL
See USERS")

  (type wire-object
    "Superclass for all classes that can be put onto the wire.")

  (type update
    "Base class for all updates.

See WIRE-OBJECT
See ID
See CLOCK
See FROM
See NEXT-ID")

  (function id
    "Accessor to the ID of the update.

IDs should be connection-unique, meaning the same ID should not
appear on different connections at the same time.

See UPDATE")

  (function clock
    "Accessor to the clock of the update.

Must be a universal-time timestamp set at the time the update
was constructed.

See UPDATE")

  (function from
    "Accessor to the sender of the update.

Must be a username string identifying the user that sent it.

See UPDATE")

  (type ping
    "Update to represent a ping request.

The recipient must reply with a PONG update.

Possible responses:
PONG

See UPDATE")

  (type pong
    "Update to represent a pong response.

See PING
See UPDATE")

  (type connect
    "Update to represent a connection request.

This update must be the first thing sent by the client upon
connection establishment.

Possible responses:
CONNECT
BAD-NAME
INVALID-PASSWORD
USERNAME-TAKEN
NO-SUCH-PROFILE
INCOMPATIBLE-VERSION

See PASSWORD
See VERSION
See UPDATE")

  (function version
    "Accessor to the version of the connection update.

See CONNECT")

  (type disconnect
    "Update to represent a disconnection request.

Possible responses:
DISCONNECT
USERNAME-MISMATCH

See UPDATE")

  (type register
    "Update to represent a registration request.

Possible responses:
REGISTER
BAD-NAME
USERNAME-MISMATCH
INSUFFICIENT-PERMISSIONS

See UPDATE")

  (type channel-update
    "Superclass for all updates relating to a channel.

See CHANNEL
See UPDATE")

  (function channel
    "Accessor to the name of the channel the update relates to.

See CHANNEL-UPDATE")

  (type target-update
    "Superclass for all updates that target a user.

See TARGET
See UPDATE")

  (function target
    "Accessor to the name of the user being targeted.

See TARGET-UPDATE")

  (type text-update
    "Superclass for all updates that carry a text string.

See TEXT
See UPDATE")

  (function text
    "Accessor to the text string carried by the update.

See TEXT-UPDATE")

  (type join
    "Update to represent a channel join request.

Possible responses:
JOIN
BAD-NAME
USERNAME-MISMATCH
ALREADY-IN-CHANNEL
NO-SUCH-CHANNEL
INSUFFICIENT-PERMISSIONS

See CHANNEL-UDPATE")

  (type leave
    "Update to represent a channel leave request.

Possible responses:
LEAVE
BAD-NAME
USERNAME-MISMATCH
NOT-IN-CHANNEL
NO-SUCH-CHANNEL
INSUFFICIENT-PERMISSIONS

See CHANNEL-UDPATE")

  (type create
    "Update to represent a channel creation request.

The channel may be NIL, in which case an anonymous channel
is constructed. You can obtain the name of the anonymous
channel by reading it out of the JOIN response.

Possible responses:
JOIN
BAD-NAME
USERNAME-MISMATCH
CHANNELNAME-TAKEN
BAD-NAME
INSUFFICIENT-PERMISSIONS

See CHANNEL-UPDATE")

  (type kick
    "Update to represent a user kick request.

Possible responses:
KICK
BAD-NAME
USERNAME-MISMATCH
NOT-IN-CHANNEL
NO-SUCH-USER
NO-SUCH-CHANNEL
INSUFFICIENT-PERMISSIONS

See TARGET-UPDATE
See CHANNEL-UPDATE")

  (type pull
    "Update to represent a user pull request.

The user will be automatically joined to the channel.

Possible responses:
JOIN
BAD-NAME
USERNAME-MISMATCH
ALREADY-IN-CHANNEL
NO-SUCH-USER
NO-SUCH-CHANNEL
INSUFFICIENT-PERMISSIONS

See TARGET-UPDATE
See CHANNEL-UPDATE")

  (type permissions
    "Update to represent a channel permissions view or change request.

If PERMISSIONS is NIL, the permissions are not changed.

Possible responses:
PERMISSIONS
BAD-NAME
USERNAME-MISMATCH
NO-SUCH-CHANNEL
INSUFFICIENT-PERMISSIONS

See PERMISSIONS
See CHANNEL-UPDATE")

  (type message
    "Update to represent a channel message request.

Possible responses:
MESSAGE
BAD-NAME
USERNAME-MISMATCH
NO-SUCH-CHANNEL
INSUFFICIENT-PERMISSIONS

See CHANNEL-UPDATE
See TEXT-UPDATE")

  (type users
    "Update to represent a channel users listing request.

Possible responses:
USERS
BAD-NAME
USERNAME-MISMATCH
NO-SUCH-CHANNEL
INSUFFICIENT-PERMISSIONS

See CHANNEL-UPDATE
See USERS")

  (type channels
    "Update to represent a channels listing request.

Possible responses:
CHANNELS
BAD-NAME
USERNAME-MISMATCH

See CHANNELS
See UPDATE")

  (type user-info
    "Update to represent a user information request.

Possible responses:
USER-INFO
BAD-NAME
USERNAME-MISMATCH
NO-SUCH-USER

See REGISTERED
See CONNECTIONS
See TARGET-UPDATE")

  (function registered
    "Accessor to whether the user is registered or not.

See USER-INFO")

  (type failure
    "Superclass for all failure response updates.

See TEXT-UDPATE")

  (type malformed-update
    "Update in response to a malformed update.

See FAILURE")

  (type connection-unstable
    "Update in response to an unstable or slow connection.

See FAILURE")

  (type too-many-connections
    "Update in response to too many connections being requested on the server globally, or for a single user.

See FAILURE")

  (type update-failure
    "Update in response to an update request that failed.

See UPDATE-ID")

  (function update-id
    "Accessor to the ID of the update that failed to be completed.

See UPDATE-FAILURE")

  (type invalid-update
    "Update in response to an update of invalid type.

See UPDATE-FAILURE")

  (type username-mismatch
    "Update in response to a mismatch between the known username and the one in the FROM field.

See UPDATE-FAILURE")

  (type incompatible-version
    "Update in response to a connection attempt with an incompatible protocol version.

See UPDATE-FAILURE")

  (type invalid-password
    "Update in response to a connection attempt with an invalid password for the profile.

See UPDATE-FAILURE")

  (type no-such-profile
    "Update in response to a connection attempt with a password for an inexistent profile.

See UPDATE-FAILURE")

  (type username-taken
    "Update in response to a connection attempt with a username that is already taken.

See UPDATE-FAILURE")

  (type no-such-channel
    "Update in response to a CHANNEL-UPDATE for a channel that does not exist.

See UPDATE-FAILURE")

  (type already-in-channel
    "Update in response to a JOIN/PULL request for a user that is already in the specified channel.

See UPDATE-FAILURE")

  (type not-in-channel
    "Update in response to a LEAVE/KICK request for a user that is not in the specified channel.")

  (type channelname-taken
    "Update in response to a CREATE request for a channel that already exists.")

  (type bad-name
    "Update in response to any named request with a name that is not valid.

Relevant are the FROM, TARGET, and CHANNEL fields.

See UPDATE-FAILURE")

  (type insufficient-permissions
    "Update in response to a request that is not permitted on the current or primary channel.

See UPDATE-FAILURE")

  (type invalid-permissions
    "Update in response to a PERMISSIONS request that attempted to set malformed permissions.

See UPDATE-FAILURE")

  (type no-such-user
    "Update in response to a TARGET-UPDATE request that refers to user that does not exist.

See UPDATE-FAILURE")

  (type too-many-updates
    "Update in response to a flooding of the server.

When this update is sent, any number of future updates
that are received may be dropped instead.

See UPDATE-FAILURE"))

;; reader.lisp
(docs:define-docs
  (variable *whitespace*
    "A vector of whitespace characters.")

  (variable *errors*
    "A list of errors that occurred during reading.")

  (variable *invalid-symbol*
    "Placeholder symbol for symbols that could not be properly read.")

  (function whitespace-p
    "Returns T if the character is considered to be whitespace.

See *WHITESPACE*")

  (function skip-whitespace
    "Consumes all characters from the stream until a non-whitespace character is found.

See WHITESPACE-P")

  (function safe-find-symbol
    "Similar to FIND-SYMBOL, but recording an error for unknown symbols.

If the symbol cannot be found, a condition of type
UNKNOWN-SYMBOL is pushed onto the *ERRORS* list and
*INVALID-SYMBOL* is returned.

See FIND-SYMBOL
See UNKNOWN-SYMBOL
See *INVALID-SYMBOL*
See *ERRORS*")

  (function read-sexpr-list
    "Read a list from the stream. It is assumed that the opening paren has already been read.")

  (function read-sexpr-string
    "Read a string from the stream. It is assumed that the opening double-quote has already been read. ")

  (function read-sexpr-keyword
    "Read a keyword from the stream. It is assumed that the colon has already been read.

See SAFE-FIND-SYMBOL
See READ-SEXPR-TOKEN")

  (function read-sexpr-number
    "Reads a number from the stream. The number can be either an integer or a float.")

  (function read-sexpr-token
    "Reads a token from the stream.

Tokens are sequences of characters that are terminated
by one of the following:
 \"():0123456789. 
These characters may still appear in a token, but must
be escaped by a backslash.")

  (function read-sexpr-symbol
    "Reads a symbol from the stream.

See READ-SEXPR-TOKEN
See SAFE-FIND-SYMBOL")

  (function read-sexpr
    "Reads an s-expression from the stream.

Skips all the whitespace at the beginning.
Depending on the character following, the reading is
dispatched as follows:
  (           => READ-SEXPR-LIST
  )           => Signals an INCOMPLETE-TOKEN error
  \"           => READ-SEXPR-STIRNG
  012345689.  => READ-SEXPR-NUMBER
  :           => READ-SEXPR-KEYWORD
  otherwise   => READ-SEXPR-SYMBOL

See READ-SEXPR-LIST
See READ-SEXPR-STRING
See READ-SEXPR-NUMBER
See READ-SEXPR-KEYWORD
See READ-SEXPR-SYMBOL"))

;; typed-slot-class.lisp
(docs:define-docs
  (variable *unbound-value*
    "Placeholder value for unbound values.")

  (function check-compatible-slot-values
    "Checks whether the given value is suitable for the given object's slot.

If it is not, an error of type INCOMPATIBLE-VALUE-TYPE-FOR-SLOT
is signalled.")

  (type typed-slot
    "Slot class for slots with a strict type check.

See SLOT-TYPE
See TYPED-DIRECT-SLOT-DEFINITION
See TYPED-EFFECTIVE-SLOT-DEFINITION
See TYPED-SLOT-CLASS")

  (function slot-type
    "Accessor to the type constraint of the typed-slot.

See TYPED-SLOT")

  (type typed-direct-slot-definition
    "Direct-slot class for typed slots.

See TYPED-SLOT")

  (type typed-effective-slot-definition
    "Effective-slot class for typed slots.

See TYPED-SLOT")

  (type typed-slot-class
    "Metaclass for classes with strictly typed slots.

See TYPED-SLOT
See TYPED-OBJECT")

  (type typed-object
    "Superclass for objects with strictly typed slots.

See TYPED-SLOT-CLASS
See DEFINE-TYPED-CLASS")

  (function define-typed-class
    "Shorthand wrapper around defclass to set it up for a typed-class.

See TYPED-OBJECT
See TYPED-SLOT-CLASS"))

;; wire.lisp
(docs:define-docs
  (function to-wire
    "Print the wireable object to the stream.

Only handles objects of type WIRE-OBJECT and WIREABLE.

WIRE-OBJECTS are printed as a list in the following
format: a list of the object's type symbol followed by
pairs of keyword to value of the object's slots that
have an initarg. Slots without an initarg are not
printed.

The output is forced once written fully.

See PRINT-SEXPR
See FORCE-OUTPUT")

  (function check-update-options
    "Checks the given sexpr for conformity to use as initargs for an update.

The items after the first symbol in the list are checked
as follows: they must be balanced pairs of KEYWORD to
atom. If this is not the case, an error of type
MALFORMED-WIRE-OBJECT is signalled. If no key :ID is
found, an error of type MISSING-ID is signalled. If no
key :CLOCK is found, an error of type MISSING-CLOCK is
signalled.

See FROM-WIRE
See MISSING-ID
See MISSING-CLOCK
See MALFORMED-WIRE-OBJECT")

  (function from-wire
    "Read a wire object from the stream.

First an object is read from stream by READ-SEXPR.
If the returned object is not a cons, it is returned
immediately. Otherwise, the procedure is as follows:
The first element must be a symbol. If it isn't, an
error of type MALFORMED-WIRE-OBJECT is signalled.
If the symbol does not designate a class, or designates
a class that is not a subclass of WIRE-OBJECT, an error
of type UNKNOWN-WIRE-OBJECT is signalled. If the class
is a subclass of UPDATE, the rest of the items in the
list are checked by CHECK-UPDATE-OPTIONS. Finally
MAKE-INSTANCE is called with the full expression as
arguments. If the class is a subclass of wire-object,
MAKE-INSTANCE is called with the full expression as
arguments immediately.

See READ-SEXPR
See MALFORMED-WIRE-OBJECT
See UNKNOWN-WIRE-OBJECT
See CHECK-UPDATE-OPTIONS"))
