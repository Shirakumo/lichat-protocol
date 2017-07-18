## About Lichat-Protocol
This system specifies and implements the protocol of the Lichat chat system. It offers both verbal and programmatical descriptions of the protocol. If you are working with a Common Lisp system, you can use this system straight-away to process Lichat messages.

## Protocol Specification
### 1. Wire Format
The wire format is based on UTF-8 character streams on which objects are serialised in a secure, simplified s-expression format. The format is as follows:

```BNF
WIREABLE ::= OBJECT | STRING | SYMBOL | NUMBER
OBJECT   ::= '(' WHITE* SYMBOL (WHITE+ KEYWORD WHITE+ EXPR)* WHITE* ')'
EXPR     ::= STRING | LIST | SYMBOL | NUMBER
STRING   ::= '"' ('\' . | !'"')* '"'
LIST     ::= '(' WHITE* (EXPR (WHITE+ EXPR)*)? WHITE* ')'
SYMBOL   ::= KEYWORD | '#' ':' NAME | NAME ':' NAME
KEYWORD  ::= ':' NAME
NUMBER   ::= '0..9'+ ( '.' '0..9'*)? | '.' '0..9'*
NAME     ::= (('\' .) | !TERMINAL)+
TERMINAL ::= ('0..9' | ':' | ' ' | '"' | '.' | '(' | ')')
WHITE    ::= U+0009 | U+000A | U+000B | U+000C | U+000D | U+0020
```

Special care must be taken when reading and printing symbols. Symbols that come from the `lichat-protocol` package must be printed without the package name prefix. Symbols from the `keyword` package must be printed by their name only prefixed by a `:`. Symbols without a package must be printed by their name only and prefixed by a `#:`. Every other symbol must be prefixed by the symbol's package's name followed by a `:`. When a symbol is read, it is checked whether it exists in the corresponding package laid out by the previous rules. If it does not exist, the expression is not valid and an error must be generated, but only after the expression has been read to completion.

Only Common Lisp objects of type `wireable` can be serialised to the wire format. Special care must also be taken when `wire-object`s are read from the wire. An error must be generated if the object is malformed by either a non-symbol in the first place of its list, imbalanced key/value pairs in the tail, or non-keywords in the key position. An error must also be generated if the symbol at the first position does not name a class that is a subclass of `wire-object`. If it is a subclass of `update`, the keys (and values) named `:id` and `:clock` must also be present, lest an error be generated.

See `to-wire`, `from-wire`.

### 2. Server Objects
The server must keep track of a number of objects that are related to the current state of the chat system. The client may also keep track of some of these objects for its own convenience.

#### 2.1 Connection
Each client is connected to the server through a `connection` object. Each connection in turn is tied to a user object. A user may have up to an implementation-dependant number of connections at the same time.

#### 2.2 User
`user`s represent participants on the chat network. A user has a globally unique name and a number of connections that can act as the user. Each user can be active in a number of channels, the maximal number of which is implementation-dependant. A user must always inhabit the primary channel. A user may have a profile object associated with it. When such a profile exists, the user is considered to be "registered." The server itself must also have an associated user object, the name of which is up to the specific server instance.

##### 2.2.1 User Name Constraints
A user's name must be between 1 and 32 characters long, where each character must be from the Unicode general categories Letter, Mark, Number, Punctuation, and Symbol, or be a Space (`U+0020`).

#### 2.3 Profile
The `profile` primarily exists to allow end-users to log in to a user through a password and thus secure the username from being taken by others. A profile has a maximal lifetime. If the user associated with the profile has not been used for longer than the profile's lifetime, the profile is deleted.

#### 2.4 Channel
`channel`s represent communication channels for users over which they can send messages to each other. A channel has a set of permission rules that constrain what kind of updates may be performed on the channel by whom. There are three types of channels that only differ in their naming scheme and their permissions.

##### 2.4.1 Primary Channels
Exactly one of these must exist on any server, and it must be named the same as the server's user. All users that are currently connected to the server must inhabit this channel. The channel may not be used for sending messages by anyone except for system administrators or the server itself. The primary channel is also used for updates that are "channel-less," to check them for permissions.

##### 2.4.2 Anonymous Channels
Anonymous channels must have a random name that is prefixed with an `@`. Their permissions must prevent users that are not already part of the channel from sending `join`, `channels`, `users`, or any other kind of update to it, thus essentially making it invisible safe for specially invited users.

##### 2.4.3 Regular Channels
Any other channel is considered a "regular channel".

##### 2.4.4 Channel Name Constraints
The names of channels are constrained in the same way as user names. See §2.2.1.

#### 2.5 Permission Rules
A permission rule specifies the restrictions of an update type on who is allowed to perform the update on the channel. The structure is as follows:

```BNF
RULE     ::= (type EXPR*)
EXPR     ::= COMPOUND | username | t | nil
COMPOUND ::= (not EXPR) | (or EXPR*) | (and EXPR*)
```

Where `type` is the name of an update class, and `username` is the name of a user object. `t` is the CL symbol `T` and indicates "anyone". `nil` is the CL symbol `NIL` and indicates "no one". The compound operators combine the expressions logically as sensible. The expressions within the rule are combined as by an `or` compound.

### 3. General Interaction
The client and the server communicate through `update` objects over a connection. Each such object that is issued from the client must contain a unique `id`. This is important as the ID is reused by the server in order to communicate replies. The client can then compare the ID of the incoming updates to find the response to an earlier request, as responses may be reordered or delayed. The server does not check the ID in any way-- uniqueness and avoidance of clashing is the responsibility of the client. Each update must also contain a `clock` slot that specifies the time of sending. This is used to calculate latency and potential connection problems.

When an update is sent to a channel, it is distributed to all the users currently in the channel. When an update is sent to a user, it is distributed to all the connections of the user. When an update is sent to a connection, it is serialised to the wire according to the above wire format specification. The actual underlying mechanism that transfers the characters of the wire format to the remote host is implementation-dependant.

### 4. Connection
#### 4.1 Establishment
After the connection between a client and a server has been established through some implementation-dependant means, the client must send a `connect` update. The update will attempt to register the user on the server, as follows:

1. If the server cannot sustain more connections, a `too-many-connections` update is returned and the connection is closed.
1. If the update's `version` denotes a version that is not compatible to the version of the protocol on the server, an `incompatible-version` update is returned and the connection is closed. 
1. If the update's `from` field contains an invalid name, a `bad-name` update is returned and the connection is closed.
1. If the update does not contain a `password`, and the `from` field denotes a username that is already taken by an active user or a registered user, an `username-taken` update is returned and the connection is closed. 
1. If the update does contain a `password`, and the `from` field denotes a username that is not registered, a `no-such-profile` update is returned and the connection is closed.
1. If the update does contain a `password`, and the `from` field denotes a username that is registered, but whose password does not match the given one, an `invalid-password` update is returned and the connection is closed.
1. If the server cannot sustain more connections for the requested user, a `too-many-connections` update is returned and the connection is closed.
1. A user corresponding in name to the `from` field is created if it does not yet exist.
1. The connection is tied to its corresponding user object.
1. The server responds with a `connect` update of the same id as the one the client sent. The `from` field must correspond to the server's user object's name.
1. If the user already existed, the server responds with `join` updates for each of the channels the user is currently inhabiting.
1. If the user did not already exist, it is joined to the primary channel.

#### 4.2 Connection Maintenance
If the `clock` of an update diverges too much from the one known by the server, the server may drop the connection after replying with a `connection-unstable` update.

The server must receive an update on a connection within at least a certain implementation-dependant interval that must be larger than 100 seconds. If this does not happen, the server may assume a disconnection and drop the client after replying with a `connection-unstable` update. If the server does not receive an update from the client within an interval of up to 60 seconds, the server must send a `ping` update to the client, to which the client must respond with a `pong` update. This is to ensure the stability of the connection.

If the client sends too many updates in too short a time interval, the server may start dropping updates, as long as it responds with a `too-many-updates` update when it starts doing so. This throttling may be sustained for an implementation-dependant length of time. The client might send occasional `ping` requests to figure out if the throttling has been lifted. The server may also close the connection if it deems the flooding too severe.

#### 4.3 Connection Closure
A connection may be closed either due to a `disconnect` update request from the client, or due to problems on the server side. When the connection is closed, the server must act as follows:

1. The server responds with a `disconnect` update, if it still can.
1. The underlying connection between the client and the server is closed.
1. The connection object is removed from the associated user object.
1. If the user does not have any remaining connections, the user leaves all channels it inhabited.

The exceptional situation being during connection establishment. If the server decides to close the connection then, it may do so without responding with a `disconnect` update and may immediately close the underlying connection.

### 5. Client Interaction
#### 5.1 General Update Checks
An update is always checked as follows:

1. If the update is not at all recognisable and cannot be parsed, a `malformed-update` update is sent back and the request is dropped.
1. If the class of the update is not known or not a subclass of `wire-object`, an `invalid-update` update is sent back and the request is dropped.
1. If the `from`, `channel`, or ` target` fields contain an invalid name, a `bad-name` update is sent back and the request is dropped.
1. If the `from` field does not match the name known to the server by the user associated to the connection, a `username-mismatch` update is sent back and the request is dropped.
1. If the `channel` field denotes a channel that does not exist, but must, a `no-such-channel` update is sent back and the request is dropped.
1. If the `target` field denotes a user that does not exist, a `no-such-user` update is sent back and the request is dropped.
1. If the update is an operation that is not permitted on its target channel, or the primary channel if no target channel is applicable, an `insufficient-permissions` update is sent back and the request is dropped.

#### 5.2 Profile Registration
When a user sends a `register` update, the server must act as follows:

1. If a profile of the same name as the user does not already exist, the profile is created.
1. The password of the profile associated to the user is changed to match the one from the update.
1. The profile must stay live until at least 30 days after the user associated with the profile has existed on the server.

Note that the server does not need to store the password verbatim, and is instead advised to only store and compare a hash of it.

#### 5.3 Channel Creation & Management
Since a channel has only two bits of information associated with it, the management of channels is rather simple. Creating a new channel happens with the `create` update:

1. The update is checked for permissions by the primary channel.
1. If a channel of the `channel` name in the update already exists, the server responds with a `channelname-taken` update and drops the request.
1. If the `channel` field is `NIL`, an anonymous channel is created, otherwise a regular channel is created.
1. The user is automatically joined to the channel.
1. The server responds with a `join` update to the user with the `id` being the same as the id of the create update.

From there on out the channel's permissions can be viewed or changed with the `permissions` update, if the channel allows you to do so. Note that the server must only update the channel's permissions, if the update's `permissions` field is not `NIL`.

See §2.5 for an explanation of the proper syntax of the permissions.

#### 5.4 Channel Interaction
A user can interact with a channel in several ways. 

##### 5.4.1 Joining a Channel
Joining a channel happens with the `join` update, after which the server acts as follows:

1. If the user is already in the named channel, an `already-in-channel` update is sent back and the request is dropped.
1. The user is added to the channel's list of users.
1. The user's `join` update is distributed to all users in the channel.

##### 5.4.2 Leaving a Channel
Leaving a channel again happens with the `leave` update, after which the server acts as follows:

1. If the user is not in the named channel, a `not-in-channel` update is sent back and the request is dropped.
1. The user's `leave` update is distributed to all users in the channel.
1. The user is removed from the channel's list of users.

##### 5.4.3 Pulling a User
Another user can be pulled into the channel by the `pull` update, after which the server acts as follows:

1. If the user is not in the named channel, a `not-in-channel` update is sent back and the request is dropped.
1. If the target user is already in the named channel, an `already-in-channel` update is sent back and the request is dropped.
1. The target user is added to the channel's list of users.
1. A `join` update for the target user with the same `id` as the `pull` update is distributed to all users in the channel.

##### 5.4.4 Kicking a User
Another user can be kicked from a channel by the `kick` update, after which the server acts as follows:

1. If the user is not in the named channel, a `not-in-channel` update is sent back and the request is dropped.
1. If the target user is not in the named channel, a `not-in-channel` update is sent back and the request is dropped.
1. The user's `kick` update is distributed to all users in the channel.
1. A `leave` update for the target user is distributed to all users in the channel.
1. The target user is removed from the channel's list of users.

##### 5.4.5 Sending a Message
Finally, a user can send a message to all other users in a channel with the `message` update, after which the server acts as follows:

1. If the user is not in the named channel, a `not-in-channel` update is sent back and the request is dropped.
1. The user's `message` update is distributed to all users in the channel.

#### 5.5 Server Information Retrieval
The server can provide a client with several pieces of information about its current state. 

##### 5.5.1 Listing Public Channels
Retrieving a list of channels can be done with the `channels` update, after which the server acts as follows:

1. For each channel known to the server, the server checks the update against the channel's permissions.
1. If the permissions allow the update, the channel's name is recorded.
1. A `channels` update with the same `id` as the request is sent back with the `channels` field set to the list of names of channels that were recorded.

##### 5.5.2 Listing All Users of a Channel
The list of users currently in a channel can be retrieved by the `users` update, after which the server acts as follows:

1. A list of the users in the channel is recorded.
1. A `users` update with the same `id` as the request is sent back with the `users` field set to the list of names of users that were recorded.

##### 5.5.3 Requesting Information About a User
Finally, information about a particular user can be retrieved by the `user-info` update, after which the server acts as follows:

1. A `user-info` update with the same `id` as the request is sent back with the `connections` field set to the number of connections the user object has associated with it and with the `registered` field set to `T` if the user has a profile associated with it.

### 6. Protocol Extension
A server or client may provide extensions to the protocol in the following manners:

* **Additional Update Types** -- If such an update is sent to a client that does not recognise it, it should be ignored. If such an update is sent to a server that does not recognise it, the server will respond with an `invalid-update`.
* **Additional Update Fields** -- A client or server may extend the existing update classes with additional, optional fields to provide further information or other kinds of behaviour. The server or client is not allowed to introduce additional required fields. When an update with unknown initargs is received, the unknown initargs are to be ignored.

## See Also

* [lichat-serverlib](https://shirakumo.github.io/lichat-serverlib) An agnostic implementation of the server-side protocol.
* [lichat-tcp-server](https://shirakumo.github.io/lichat-tcp-server) A basic, threaded, TCP-based implementation of a Lichat server.
* [lichat-tcp-client](https://shirakumo.github.io/lichat-tcp-client) A basic, threaded, TCP-based implementation of a Lichat client.
* [LionChat](https://github.com/Shirakumo/lionchat) A Qt GUI client for a TCP server.
