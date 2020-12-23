## About Lichat-Protocol
This system specifies and implements the protocol of the Lichat chat system. It offers both verbal and programmatical descriptions of the protocol. If you are working with a Common Lisp system, you can use this system straight-away to process Lichat messages.

## Protocol Specification
### 1. Wire Format
The wire format is based on UTF-8 character streams on which objects are serialised in a secure, simplified s-expression format. The format is as follows:

```BNF
UPDATE   ::= OBJECT NULL
OBJECT   ::= '(' WHITE* SYMBOL (WHITE+ KEYWORD WHITE+ EXPR)* WHITE* ')'
EXPR     ::= STRING | LIST | SYMBOL | NUMBER
STRING   ::= '"' ('\' ANY | !('"' | NULL))* '"'
LIST     ::= '(' WHITE* (EXPR (WHITE+ EXPR)*)? WHITE* ')'
SYMBOL   ::= KEYWORD | NAME ':' NAME
KEYWORD  ::= ':' NAME
NUMBER   ::= '0..9'+ ( '.' '0..9'*)? | '.' '0..9'*
NAME     ::= (('\' ANY) | !(TERMINAL | NULL))+
TERMINAL ::= (':' | ' ' | '"' | '.' | '(' | ')')
WHITE    ::= U+0009 | U+000A | U+000B | U+000C | U+000D | U+0020
NULL     ::= U+0000
ANY      ::= !NULL
```

See `to-wire`, `from-wire`.

#### 1.1 Symbols
Special care must be taken when reading and printing symbols. Symbols that come from the `lichat-protocol` package must be printed without the package name prefix. Symbols from the `keyword` package must be printed by their name only prefixed by a `:`. Every other symbol must be prefixed by the symbol's package's name followed by a `:`. When a symbol is read, it is checked whether it exists in the corresponding package laid out by the previous rules. If it does not exist, the expression is not valid and an error must be generated, but only after the expression has been read to completion.

#### 1.2 Objects
Only Common Lisp objects of type `wireable` can be serialised to the wire format. Special care must also be taken when `wire-object`s are read from the wire. An error must be generated if the object is malformed by either a non-symbol in the first place of its list, imbalanced key/value pairs in the tail, or non-keywords in the key position. An error must also be generated if the symbol at the first position does not name a class that is a subclass of `wire-object`. If it is a subclass of `update`, the keys (and values) named `:id` and `:clock` must also be present, lest an error be generated.

#### 1.3 Null Characters
Null characters (`U+0000`) must not appear anywhere within a wireable. If a string were to contain null characters, they must be filtered out. If a symbol were to contain null characters, the message may not be put to the wire.

### 2. Server Objects
The server must keep track of a number of objects that are related to the current state of the chat system. The client may also keep track of some of these objects for its own convenience.

#### 2.1 Connection
Each client is connected to the server through a `connection` object. Each connection in turn is tied to a user object. A user may have up to an implementation-dependant number of connections at the same time.

#### 2.2 User
`user`s represent participants on the chat network. A user has a globally unique name and a number of connections that can act as the user. Each user can be active in a number of channels, the maximal number of which is implementation-dependant. A user must always inhabit the primary channel. A user may have a profile object associated with it. When such a profile exists, the user is considered to be "registered." The server itself must also have an associated user object, the name of which is up to the specific server instance.

##### 2.2.1 User Name Constraints
A user's name must be between 1 and 32 characters long, where each character must be from the Unicode general categories Letter, Mark, Number, Punctuation, and Symbol, or be a Space (`U+0020`). The name must not begin or end with 
with Spaces (`U+0020`). Two user names are considered the same if they are the same length and each code point matches case-insensitively.

#### 2.3 Profile
The `profile` primarily exists to allow end-users to log in to a user through a password and thus secure the username from being taken by others. A profile has a maximal lifetime. If the user associated with the profile has not been used for longer than the profile's lifetime, the profile is deleted.

##### 2.3.1 Password Constraints
A profile's password must be at least 6 characters long. It may contain any kind of character that is not Null (`U+0000`).

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
RULE     ::= (type EXPR)
EXPR     ::= t | nil | EXCLUDE | INCLUDE
EXCLUDE  ::= (- username*)
INCLUDE  ::= (+ username*)
```

Where `type` is the name of an update class, and `username` is the name of a user object. `t` is the CL symbol `T` and indicates "anyone". `nil` is the CL symbol `NIL` and indicates "no one". The `INCLUDE` expression only allows users whose names are listed to perform the action. The `EXCLUDE` expression allows anyone except users whose names are listed to perform the action. The expression `t` is thus equivalent to `(-)` and the expression `nil` is equivalent to `(+)`.

### 3. General Interaction
The client and the server communicate through `update` objects over a connection. Each such object that is issued from the client must contain a unique `id`. This is important as the ID is reused by the server in order to communicate replies. The client can then compare the ID of the incoming updates to find the response to an earlier request, as responses may be reordered or delayed. The server does not check the ID in any way-- uniqueness and avoidance of clashing is the responsibility of the client. Each update must also contain a `clock` slot that specifies the time of sending. This is used to calculate latency and potential connection problems.

When an update is sent to a channel, it is distributed to all the users currently in the channel. When an update is sent to a user, it is distributed to all the connections of the user. When an update is sent to a connection, it is serialised to the wire according to the above wire format specification. The actual underlying mechanism that transfers the characters of the wire format to the remote host is implementation-dependant.

#### 3.1 Null Termination of Updates
At the end of each update there has to be a single null character (`U+0000`). This character can be used to distinguish individual updates on the wire and may serve as a marker to attempt and stabilise the stream in case of malformed updates or other problems that might occur on the lower level.

### 4. Connection
#### 4.1 Connection Establishment
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

Should the user send a `connect` update after already having completed the connection handshake above, the server must drop the update and respond with an `already-connected` update.

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
1. If the update is too long (contains too many characters), a `update-too-long` update is sent back and the request is dropped.
1. If the class of the update is not known or not a subclass of `wire-object`, an `invalid-update` update is sent back and the request is dropped.
1. If the `from`, `channel`, or ` target` fields contain an invalid name, a `bad-name` update is sent back and the request is dropped.
1. If the `from` field does not match the name known to the server by the user associated to the connection, a `username-mismatch` update is sent back and the request is dropped.
1. If the `channel` field denotes a channel that does not exist, but must, a `no-such-channel` update is sent back and the request is dropped.
1. If the `target` field denotes a user that does not exist, a `no-such-user` update is sent back and the request is dropped.
1. If the update is an operation that is not permitted on its target channel, or the primary channel if no target channel is applicable, an `insufficient-permissions` update is sent back and the request is dropped.

#### 5.2 Profile Registration
When a user sends a `register` update, the server must act as follows:

1. If the server disagrees with the attempted registration, a `registration-rejected` update is sent back and the request is dropped.
1. If the profile does not yet exist, it is created.
1. The password of the profile associated to the user is changed to match the one from the update.
1. The profile must stay live until at least 30 days after the user associated with the profile has existed on the server.
1. The server responds by sending back the original `register` update.

Note that the server does not need to store the password verbatim, and is instead advised to only store and compare a hash of it.

#### 5.3 Channel Creation & Management
Since a channel has only two bits of information associated with it, the management of channels is rather simple.

##### 5.3.1 Creating a Channel
Creating a new channel happens with the `create` update:

1. The update is checked for permissions by the primary channel.
1. If a channel of the `channel` name in the update already exists, the server responds with a `channelname-taken` update and drops the request.
1. If the `channel` field is `NIL`, an anonymous channel is created, otherwise a regular channel is created.
1. The user is automatically joined to the channel.
1. The server responds with a `join` update to the user with the `id` being the same as the id of the create update.

##### 5.3.2 Updating a Channel
The channel's permissions can be viewed or changed with the `permissions` update, if the channel allows you to do so.

1. The permissions for the channel are updated with the ones specified in the update's `permissions` field as follows:
   1. For each rule in the specified permissions set in the update:
   1. If the rule should be malformed or unacceptable, the server responds with a `invalid-permissions` update and disregards the rule.
   1. Otherwise, set the rule with the same type in the channel's rule set to the given rule.
1. The server responds with a `permissions` update with the `permissions` field set to the full permissions set of the channel, and the `id` being the same as the id of the update the user sent.

See §2.5 for an explanation of the proper syntax of the permissions. Note that the server may reduce the set of rules to a simpler set that is semantically equivalent.

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

1. If the user is not in the named channel, a `not-in-channel` update is sent back and the request is dropped.
1. A list of the users in the channel is recorded.
1. A `users` update with the same `id` as the request is sent back with the `users` field set to the list of names of users that were recorded.

##### 5.5.3 Requesting Information About a User
Finally, information about a particular user can be retrieved by the `user-info` update, after which the server acts as follows:

1. If the user is not connected and no profile for the user exists, a `no-such-user` update is sent back and the request is dropped.
1. A `user-info` update with the same `id` as the request is sent back with the `connections` field set to the number of connections the user object has associated with it and with the `registered` field set to `T` if the user has a profile associated with it.

### 6. Protocol Extension
A server or client may provide extensions to the protocol in the following manners:

* **Additional Update Types** -- If such an update is sent to a client that does not recognise it, it should be ignored. If such an update is sent to a server that does not recognise it, the server will respond with an `invalid-update`.
* **Additional Update Fields** -- A client or server may extend the existing update classes with additional, optional fields to provide further information or other kinds of behaviour. The server or client is not allowed to introduce additional required fields. When an update with unknown initargs is received, the unknown initargs are to be ignored.
* **Additional Constraints** -- An extension may introduce additional constraints and restrictions on whether existing updates are considered valid.

Each extension to the protocol should receive a unique name of the form `producer-name` where `producer` is an identifier for who wrote up the extension's protocol, and `name` should be a name for the extension itself. For each extension that a server and client support, they must include the unique name of it as a string in the `connect` update's `extensions` list.

### 7. Protocol Extensions
The extensions outlined in this section are not mandatory and a server or client may choose not to implement them.

#### 7.1 Backfill (shirakumo-backfill)
A new update type called `backfill` is introduced, which is a `channel-update`. If the server receives such an update from a connection, it reacts as follows:

1. If the user is not in the named channel, a `not-in-channel` update is sent back and the request is dropped.
1. Following this, updates are sent back to the connection the update came from. These updates should include all updates that were distributed to users in the channel, spanning from now to an arbitrary point in time that is at most when the user of this connection last joined the channel. The fields of the updates must be the equal to the first time the update was sent out. The initial event of the user that requested the backfill joining the channel cannot be sent back.

The purpose of this extension is to allow users to catch up with the contents of a channel should they initiate a new connection which does not currently have access to all the past updates of the channel. In order to facilitate this, the server is forced to keep copies of the updates. The server is allowed to only keep updates for a certain duration, or only a certain number of total updates. In order to avoid spying, the server must not distribute updates that the user did not already receive previously through another connection. The server does not have to make any guarantee about the order in which the updates are sent back to the connection. The client on the other side is responsible for ordering them as appropriate according to the clock.

#### 7.2 Data (shirakumo-data)
A new update type called `data` is introduced, which is a `channel-update`. Additionally, a new `failure` type called `bad-content-type` is introduced, which is an `update-failure`. If the server receives a `data` update from a connection, it reacts as follows:

1. If the user is not in the named channel, a `not-in-channel` update is sent back and the request is dropped.
1. If the update's `content-type` is not accepted by the server, a `bad-content-type` update is sent back and the request is dropped.
1. The user's `data` update is distributed to all users in the channel.

The `data` update contains three slots, with the following intentions:

* `content-type` A string representing the [content type](https://en.wikipedia.org/wiki/Media_type) of the payload data contained in the update.
* `filename` A string representing an arbitrary name given to the payload data.
* `payload` A base-64 encoded string of binary data payload.

The purpose of this extension is to allow users to send binary data over channels. Particularly, the intention is to allow embedding of images, audio, video, and other media.

#### 7.3 Emotes (shirakumo-emotes)
Two new update types called `emotes` and `emote` are introduced. If the server receives an `emotes` update from a connection, it reacts as follows:

1. The server computes a set difference between the known emote names, and the names listed in the event's `names` slot. Emote names are case-insensitive.
1. For each emote in the calculated set, the server sends back an `emote` update, where the `name` is set to the emote's name, and the `payload` is set to the base-64 encoded image representing the emote. The `content-type` must be set accordingly.

When the client receives an `emote` update from the server, it reacts as follows:

1. The `payload` and `content-type` are associated with the `name` and persisted on the client. When the client sends an `emotes` event it to the server it should include the name of this emote in the `names` list.

The `emotes` update contains one slot, with the following intentions:

* `names` This contains a list of strings denoting the names of emotes the client knows about.

The `emote` update contains three slots, with the following intentions:

* `content-type` A string representing the [content type](https://en.wikipedia.org/wiki/Media_type) of the emote image contained in the update.
* `name` A string representing the name of the emote.
* `payload` A base-64 encoded string of binary data payload.

When the client sees a `message` update, every match of the regex `:([^:]+):` in the `text` where the group matched by the regex is the name of an emote from the known list of emotes, then the match of the regex should be displayed to the user by an image of the emote's image.

The purpose of this extension is to allow the server manager to configure emote images for the users to use, similar in functionality to what is often found on forums and other platforms.

#### 7.4 Edit (shirakumo-edit)
A new update type called `edit` is introduced, which is a `message`. If the server receives an `edit` update it acts in the same way as a regular `message` event. No additional support from the server is required outside of recognising and accepting the type.

When the client sees an `edit` update, it should change the `text` of the `message` update with the same `from` and `id` fields to the one from the `edit` update. Ideally a user interface for Lichat should also include an indication that the previous message event has been changed, including perhaps even a history of all the potential edits of a message.

If the client receives an `edit` update whose `id` and `from` fields do not refer to any previous `message` update, the client should simply ignore the update.

#### 7.5 Channel Trees (shirakumo-channel-trees)
A new convention for channel names is introduced. §2.4.4 is restricted further in the following manner: forward slash characters (`U+002F`) may only occur between two other characters that are not a forward slash character.

Generally for a `channel-update`, the following terminology is introduced: the `parent channel` is a channel with the name of the current channel up to and excluding the last forward slash character in the name. If no forward slash occurs in the name, the primary channel is considered the parent channel.

A new error `no-such-parent-channel` is introduced. It is an `update-failure`.

§5.3 is modified as follows: instead of point `1`: The update is checked for permissions by the parent channel. After point `2`: If the parent channel does not exist, the server responds with a `no-such-parent-channel` update and drops the request.

§5.5.1 is modified as follows: the `channels` update is upgraded to a `channel-update` and as such contains a `channel` field. When processing the `channels` update, the server should only process channels whose names begin with the name mentioned in the `channel` field followed by a forward slash and do not contain any further forward slashes.

Specifically, an update requesting ``foo`` should list ``foo/bar``, but not ``foo/bar/baz``.

Clients that support this extension are required to implement the following special semantic: if a user uses a command that requires a channel name, and the user begins the channel name with a forward slash, the client should automatically prepend the current channel name to the specified channel name, if there is a channel that is considered "current". If no channel is explicitly current, the primary channel is considered current.

## See Also

* [lichat-serverlib](https://shirakumo.github.io/lichat-serverlib) An agnostic implementation of the server-side protocol.
* [lichat-tcp-server](https://shirakumo.github.io/lichat-tcp-server) A basic, threaded, TCP-based implementation of a Lichat server.
* [lichat-tcp-client](https://shirakumo.github.io/lichat-tcp-client) A basic, threaded, TCP-based implementation of a Lichat client.
* [LionChat](https://github.com/Shirakumo/lionchat) A Qt GUI client for a TCP server.
* [Ocelot](https://github.com/Shirakumo/ocelot) An Android client for Lichat.
* [ex-lichat](https://github.com/Shirakumo/ex-lichat) An Elixir server for Lichat.
