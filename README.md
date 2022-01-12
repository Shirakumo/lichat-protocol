## About Lichat-Protocol
This system specifies and implements the protocol of the Lichat chat system. It offers both verbal and programmatical descriptions of the protocol. If you are working with a Common Lisp system, you can use this system straight-away to process Lichat messages.

## Protocol Specification
### 1. Wire Format & Data Types
The Lichat protocol defines the following basic data type hierarchy:

- `number`
  - `integer`
    - `time` An integer representing time as the number of seconds since 1900.1.1 1:0:0 UTC.
  - `float`
  - `id`
- `symbol`
  - `keyword` A symbol whose package is `keyword`.
  - `boolean`
    - `null` The symbol `NIL` from the `lichat` package.
    - `true` The symbol `T` from the `lichat` package.
- `list`
  - `(list type)` A list with each element being of type `type`.
- `string`
  - `username` See §2.2.1
  - `channelname` See §2.4.4
  - `password` See §2.3.1
- `object`

The wire format is based on UTF-8 character streams on which objects are serialised in a secure, simplified s-expression format. The format is as follows:

```BNF
UPDATE   ::= OBJECT NULL
OBJECT   ::= '(' WHITE* SYMBOL (WHITE+ SYMBOL WHITE+ EXPR)* WHITE* ')'
EXPR     ::= STRING | LIST | SYMBOL | NUMBER
STRING   ::= '"' ('\' ANY | !('"' | NULL))* '"'
LIST     ::= '(' WHITE* (EXPR (WHITE+ EXPR)*)? WHITE* ')'
SYMBOL   ::= KEYWORD | NAME | NAME ':' NAME
KEYWORD  ::= ':' NAME
NUMBER   ::= '0..9'+ ( '.' '0..9'*)? | '.' '0..9'*
NAME     ::= (('\' ANY) | !(TERMINAL | NULL))+
TERMINAL ::= (':' | ' ' | '"' | '.' | '(' | ')')
WHITE    ::= U+0009 | U+000A | U+000B | U+000C | U+000D | U+0020
NULL     ::= U+0000
ANY      ::= !NULL
```

#### 1.1 Symbols
A symbol is an object with a "name" that is home to a "package". A "package" is a simple collection of symbols. Symbols are uniquely identified by their name and package, and no two symbols with the same name may exist in the same package. The package and symbol names are case-insensitive, and two names are considered the same if they match after both have been transformed to lower case.

This protocol specifies two core packages: `lichat` and `keyword`. Symbols that come from the `lichat` package must be printed without the package name prefix. Symbols from the `keyword` package must be printed by their name only prefixed by a `:`. Every other symbol must be prefixed by the symbol's package's name followed by a `:`. 

When a symbol is read, it is checked whether it exists in the corresponding package laid out by the previous rules. If it does not exist, it may be substituted for a placeholder symbol. Servers must take special care not to keep symbol objects they don't know around, to avoid letting clients overload the server's memory with inexistent symbols.

#### 1.2 Objects
Special care must be taken when `object`s are read from the wire. An error must be generated if the object is malformed by either a non-symbol in the first place of its list, imbalanced key/value pairs in the tail, or non-keywords in the key position. An error must also be generated if the symbol at the first position does not name a class that is a subclass of `update`.

#### 1.3 Null Characters
Null characters (`U+0000`) must not appear anywhere within a wireable. If a string were to contain null characters, they must be filtered out. If a symbol were to contain null characters, the message may not be put to the wire.

#### 1.4 Machine-Readable Definition
All object types are specified in [lichat.sexpr](lichat.sexpr) in a machine-readable format based on the above wire format. The same format *should* also be used by extension providers to define their extensions to the protocol. The definitions can be parsed by parsing the file into a sequence of `EXPR`s, each being further parsed according to the following `DEFINITION` rule:

```BNF
DEFINITION           ::= PACKAGE | OBJECT | EXTENSION
PACKAGE              ::= (define-package PACKAGE-NAME)
PACKAGE-NAME         ::= STRING
OBJECT               ::= (define-object CLASS-NAME (SUPERCLASS*) SLOT*)
CLASS-NAME           ::= SYMBOL
SUPERCLASS           ::= SYMBOL
SLOT                 ::= (SLOT-NAME TYPE OPTIONAL?)
FIELD-NAME           ::= SYMBOL
TYPE                 ::= number | integer | time | float | id | symbol | keyword
                       | boolean | null | true | list | (list TYPE) | string
                       | username | channelname | password | object | T
OPTIONAL             ::= :optional
EXTENSION            ::= (define-extension EXTENSION-NAME EXTENSION-DEFINITION*)
EXTENSION-NAME       ::= STRING
EXTENSION-DEFINITION ::= OBJECT | OBJECT-EXTENSION
OBJECT-EXTENSION     ::= (define-object-extension CLASS-NAME (SUPERCLASS*) SLOT*)
```

The meaning of each definition is as follows:
- `package` Introduces a new, known package with the `PACKAGE-NAME` as its name.
- `object` Introduces a new object type using `CLASS-NAME` as its name. Each `SUPERCLASS` must name an object type that was previously introduced, and whose fields *and behaviour* should be inherited. Each `SLOT` defines a slot that the object holds in addition to the inherited ones. If the slot definition includes the `:optional` keyword, the slot may be omitted when serialising to the wire. If `:optional` is not included, the slot *must* be serialised. When translating from the wire, an omitted `:optional` slot should be initialised to an empty value. An omitted non-`:optional` slot must result in a `malformed-update` error.
- `extension` Introduces a protocol extension of the given `EXTENSION-NAME`. Its body includes new definitions that need to be added if the extension is to be supported.
- `object-extension` Changes an existing object type of `CLASS-NAME` by either introducing additional `SUPERCLASS`es, or introducing additional `SLOT` definitions on the object. Note that in order to stay backwards compatible, every slot specified via an `object-extension` must be `:optional`.

### 2. Server Objects
The server must keep track of a number of objects that are related to the current state of the chat system. The client may also keep track of some of these objects for its own convenience.

#### 2.1 Connection
Each client is connected to the server through a `connection` object. Each connection in turn is tied to a user object. A user may have up to an implementation-dependant number of connections at the same time.

#### 2.2 User
`user`s represent participants on the chat network. A user has a globally unique name and a number of connections that can act as the user. Each user can be active in a number of channels, the maximal number of which is implementation-dependant. A user must always inhabit the primary channel. A user may have a profile object associated with it. When such a profile exists, the user is considered to be "registered." The server itself must also have an associated user object, the name of which is up to the specific server instance.

##### 2.2.1 User Name Constraints
A user's name must be between 1 and 32 characters long, where each character must be from the Unicode general categories Letter, Mark, Number, Punctuation, and Symbol, or be a Space (`U+0020`). The name must not begin or end with 
with Spaces (`U+0020`), nor may two Spaces be consecutive. Two user names are considered the same if they are the same length and each code point matches case-insensitively.

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

Where `type` is the name of an update class, and `username` is the name of a user object. `t` is the symbol `T` and indicates "anyone". `nil` is the symbol `NIL` and indicates "no one". The `INCLUDE` expression only allows users whose names are listed to perform the action. The `EXCLUDE` expression allows anyone except users whose names are listed to perform the action. The expression `t` is thus equivalent to `(-)` and the expression `nil` is equivalent to `(+)`.

### 3. General Interaction
The client and the server communicate through `update` objects over a connection. Each such object that is issued from the client must contain a unique `id`. This is important as the ID is reused by the server in order to communicate replies. The client can then compare the ID of the incoming updates to find the response to an earlier request, as responses may be reordered or delayed. The server does not check the ID in any way-- uniqueness and avoidance of clashing is the responsibility of the client. Each update *should* also contain a `clock` slot that specifies the time of sending. This is used to calculate latency and potential connection problems. If no clock is specified, the server must substitute the current time. Finally, each update *may* contain a `from` slot to identify the sending user. If the `from` slot is not given, the server automatically substitutes the known username for the connection the update is coming from.

When an update is sent to a channel, it is distributed to all the users currently in the channel. When an update is sent to a user, it is distributed to all the connections of the user. When an update is sent to a connection, it is serialised to the wire according to the above wire format specification. The actual underlying mechanism that transfers the characters of the wire format to the remote host is implementation-dependant.

#### 3.1 Null Termination of Updates
At the end of each update there has to be a single null character (`U+0000`). This character can be used to distinguish individual updates on the wire and may serve as a marker to attempt and stabilise the stream in case of malformed updates or other problems that might occur on the lower level.

### 4. Connection
#### 4.1 Connection Establishment
After the connection between a client and a server has been established through some implementation-dependant means, the client must send a `connect` update. The update will attempt to register the user on the server, as follows:

1. If the server cannot sustain more connections, a `too-many-connections` update is returned and the connection is closed.
1. If the update's `version` denotes a version that is not compatible to the version of the protocol on the server, an `incompatible-version` update is returned and the connection is closed.
1. If the update's `from` field is missing or `NIL`, the server substitutes a *random* name that is not currently used by any other user or registered profile.
1. If the update's `from` field contains an invalid name, a `bad-name` update is returned and the connection is closed.
1. If the update does not contain a `password`, and the `from` field denotes a username that is already taken by an active user or a registered user, an `username-taken` update is returned and the connection is closed. 
1. If the update does contain a `password`, and the `from` field denotes a username that is not registered, a `no-such-profile` update is returned and the connection is closed.
1. If the update does contain a `password`, and the `from` field denotes a username that is registered, but whose password does not match the given one, an `invalid-password` update is returned and the connection is closed.
1. If the server cannot sustain more connections for the requested user, a `too-many-connections` update is returned and the connection is closed.
1. A user corresponding in name to the `from` field is created if it does not yet exist.
1. The connection is tied to its corresponding user object.
1. The server responds with a `connect` update of the same id as the one the client sent. The `from` field must correspond to the user's actual name.
1. If the user already existed, the server responds with `join` updates for each of the channels the user is currently inhabiting, with the primary channel always being the first.
1. If the user did not already exist, it is joined to the primary channel.

Should the user send a `connect` update after already having completed the connection handshake above, the server must drop the update and respond with an `already-connected` update.

#### 4.2 Connection Maintenance
If the `clock` of an update diverges too much, the server may respond with a `clock-skewed` update and correct the timestamp. If the skew varies a lot, the server may drop the connection after replying with a `connection-unstable` update.

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
1. If the user already inhabits the maximum amount of channels, the server responds with a `too-many-channels` update and drops the request.
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

As a shortcut, permissions can also be managed with the `grant` and `deny` updates, which change an individual rule. When a server receives a `grant` update, it must update the corresponding rule as follows:

1. If the rule is `T`, nothing is done.
1. If the rule is `NIL`, it is changed to `(+ user)`.
1. If the rule is an exclusion mask, `user` is removed from the mask.
1. If the rule is an inclusion mask, `user` is added to the mask if they are not already on it.

When the server receives a `deny` update, it must update the corresponding rule as follows:

1. If the rule is `T`, it is changed to `(- user)`.
1. If the rule is `NIL`, nothing is done.
1. If the rule is an exclusion mask, `user` is added to the mask if they are not already on it.
1. If the rule is an inclusion mask, `user` is removed from the mask.

After processing either a `grant` or `deny` update successfully, the update is sent back to the user.

#### 5.4 Channel Interaction
A user can interact with a channel in several ways. 

##### 5.4.1 Joining a Channel
Joining a channel happens with the `join` update, after which the server acts as follows:

1. If the user is already in the named channel, an `already-in-channel` update is sent back and the request is dropped.
1. If the user already inhabits the maximum amount of channels, the server responds with a `too-many-channels` update and drops the request.
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
1. If the target already inhabits the maximum amount of channels, the server responds with a `too-many-channels` update and drops the request.
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
Information about a particular user can be retrieved by the `user-info` update, after which the server acts as follows:

1. If the user is not connected and no profile for the user exists, a `no-such-user` update is sent back and the request is dropped.
1. A `user-info` update with the same `id` as the request is sent back with the `connections` field set to the number of connections the user object has associated with it and with the `registered` field set to `T` if the user has a profile associated with it.

##### 5.5.4 Requesting Capabilities
A user can request a list of updates they are allowed to send to a particular channel using the `capabilities` update. The server must act as follows:

1. If the user is not in the named channel, a `not-in-channel` update is sent back and the request is dropped.
1. For every update type known to the server, the server checks whether the user is permitted according to the channel's permission rule for the update. If permitted, the update type is added to a list.
1. A `capabilities` update with the same `id` as the request is sent back with the `permitted` field set to the list of updates gathered in step 2.

##### 5.5.5 Requesting Private Information
Servers may store additional information about a user, such as statistics, IP addresses, and so forth. Such information can be requested through the `server-info` update. After receiving such an update, the server must act as follows:

1. If the user is not connected and no profile for the user exists, a `no-such-user` update is sent back and the request is dropped.
1. A `server-info` update with the same `id` as the request is sent back with the `attributes` field set to an "association list", and the `connections` field set to a list of "association lists", one such "association list" per connection the user has. An association list is a list where each element has the following structure:
   - `(SYMBOL EXPR)`, where:
   - `SYMBOL` is a symbol naming the attribute that is being returned.
   - `EXPR` is the value for the attribute being returned.

The attributes being returned are dependent on the server and the supported protocol extensions. The set of returned attributes may also differ depending on the user being requested, especially if the user is the server's user.

The following attributes are specified:

- `channels` A list of channel names in which the user resides.
- `registered-on` When the user registered their profile. `NIL` if they did not.

The following connection attributes are specified:

- `connected-on` The time at which the connection was initiated.

While these attributes are specified in their purpose, a server does not have to return them.

### 6. Protocol Extension
A server or client may provide extensions to the protocol in the following manners:

* **Additional Update Types** -- If such an update is sent to a client that does not recognise it, it should be ignored. If such an update is sent to a server that does not recognise it, the server will respond with an `invalid-update`.
* **Additional Update Fields** -- A client or server may extend the existing update classes with additional, optional fields to provide further information or other kinds of behaviour. The server or client is not allowed to introduce additional required fields. When an update with unknown initargs is received, the unknown initargs are to be ignored. An extension may only add existing, well-specified initargs in keyword form (`:text`, `:channel`, `:target`, `:update-id`). Other additional initargs must be symbols from an extension-owned package.
* **Additional Constraints** -- An extension may introduce additional constraints and restrictions on whether existing updates are considered valid.
* **Additional User Attributes** -- An extension may specify additional attributes stored in profiles and returned through `server-info`.

Each extension to the protocol should receive a unique name of the form `producer-name` where `producer` is an identifier for who wrote up the extension's protocol, and `name` should be a name for the extension itself. For each extension that a server and client support, they must include the unique name of it as a string in the `connect` update's `extensions` list. Each producer also owns a symbol package with the producer's name, in which they may freely specify new symbols.

### 7. Protocol Extensions
The extensions outlined in this section are not mandatory and a server or client may choose not to implement them.

#### 7.1 Backfill (shirakumo-backfill)
Purpose: allow users to catch up with the contents of a channel should they initiate a new connection which does not currently have access to all the past updates of the channel. 

In order to facilitate this, the server is forced to keep copies of the updates. The server is allowed to only keep updates for a certain duration, or only a certain number of total updates. In order to avoid spying, the server should not distribute updates that the user did not already receive previously through another connection. The server does not have to make any guarantee about the order in which the updates are sent back to the connection. The client on the other side is responsible for ordering them as appropriate according to the clock.

A new update type called `backfill` is introduced, which is a `channel-update` and has an extra, optional field called `since` which should be a universal-time timestamp. If the server receives such an update from a connection, it reacts as follows:

1. If the user is not in the named channel, a `not-in-channel` update is sent back and the request is dropped.
1. Following this, updates are sent back to the connection the update came from. These updates should include all updates that were distributed to users in the channel, spanning from now to an arbitrary point in time that is at most when the user of this connection last joined the channel and at most the specified `since` timestamp. The fields of the updates must be the equal to the first time the update was sent out. The initial event of the user that requested the backfill joining the channel cannot be sent back.

#### 7.2 Data (shirakumo-data)
Purpose: allows distributing images and other binary payloads.

A new update type called `data` is introduced, which is a `channel-update`. Additionally, a new `failure` type called `bad-content-type` is introduced, which is an `update-failure`. If the server receives a `data` update from a connection, it reacts as follows:

1. If the user is not in the named channel, a `not-in-channel` update is sent back and the request is dropped.
1. If the update's `content-type` is not accepted by the server, a `bad-content-type` update is sent back and the request is dropped.
1. The user's `data` update is distributed to all users in the channel.

The `data` update contains three slots, with the following intentions:

* `content-type` A string representing the [content type](https://en.wikipedia.org/wiki/Media_type) of the payload data contained in the update.
* `filename` A string representing an arbitrary name given to the payload data.
* `payload` A base-64 encoded string of binary data payload.

#### 7.3 Emotes (shirakumo-emotes)
Purpose: introduces server-side emoticons that are distributed to all users for use on the client-side.

Any non-anonymous channel created by a registered user holds an `emotes` map. The server may restrict the size of this map. The server should also set the default permissions for `emotes` to anyone, and `emote` to only the registrant.

Two new update types called `emotes` and `emote` are introduced, both of which are `channel-update`s.

The `emotes` update contains one extra slot, with the following intentions:

* `names` This contains a list of strings denoting the names of emotes the client knows about.

The `emote` update contains three extra slots, with the following intentions:

* `content-type` A string representing the [content type](https://en.wikipedia.org/wiki/Media_type) of the emote image contained in the update.
* `name` A string representing the name of the emote.
* `payload` A base-64 encoded string of binary data payload.

If the server receives an `emotes` update from a connection, it reacts as follows:

1. If the channel field is missing, the primary channel's name is substituted.
1. If the user is not in the named channel, a `not-in-channel` update is sent back and the request is dropped.
1. If the channel is anonymous or wasn't created by a registered user, an `insufficient-permissions` update is sent back and the request is dropped.
1. The server computes a set difference between the known emote names for the channel, and the names listed in the event's `names` slot. Emote names are case-insensitive.
1. For each emote in the calculated set, the server sends back an `emote` update, where the `name` is set to the emote's name, the `channel` to the channel, and the `payload` is set to the base-64 encoded image representing the emote. The `content-type` must be set accordingly.
1. The server sends back the original `emotes` update, having set the `names` field to the list of known emotes for this channel.

If the server receives an `emote` update from a connection, it reacts as follows:

1. If the user is not in the named channel, a `not-in-channel` update is sent back and the request is dropped.
1. If the `name` is not yet contained in the channel's `emotes` map, and the map already matches the size restriction of the server, an `emote-list-full` update is sent back and the request is dropped.
1. If the `content-type` is not acceptable, an update of type `bad-content-type` is sent back and the request is dropped.
1. If the payload exceeds internal limits, an update of type `update-too-long` is sent back and the request is dropped.
1. If the payload is empty, the emote is removed, and otherwise the emote data is stored in the channel's `emotes` map.
1. The `emote` update is distributed to all users in the channel.

When the client receives an `emote` update from the server, it reacts as follows:

1. The `payload` and `content-type` are associated with the `name` and `channel`, and are persisted on the client. When the client sends an `emotes` event for the channel to the server it should include the name of this emote in the `names` list.

When the client sees a `message` update, every match of the regex `:([^:]+):` in the `text` where the group matched by the regex is the name of an emote from the known list of emotes for the current channel or the primary channel, then the match of the regex should be displayed to the user by an image of the emote's image.

#### 7.4 Edit (shirakumo-edit)
Purpose: allows users to make retroactive edits to their messages.

A new update type called `edit` is introduced, which is a `message`. If the server receives an `edit` update it acts in the same way as a regular `message` event. No additional support from the server is required outside of recognising and accepting the type.

When the client sees an `edit` update, it should change the `text` of the `message` update with the same `from` and `id` fields to the one from the `edit` update. Ideally a user interface for Lichat should also include an indication that the previous message event has been changed, including perhaps even a history of all the potential edits of a message.

If the client receives an `edit` update whose `id` and `from` fields do not refer to any previous `message` update, the client should simply ignore the update.

#### 7.5 Channel Trees (shirakumo-channel-trees)
Purpose: enforces a structure on channels and allows creating channel hierarchies for easier grouping.

A new convention for channel names is introduced. §2.4.4 is restricted further in the following manner: forward slash characters (`U+002F`) may only occur between two other characters that are not a forward slash character.

Generally for a `channel-update`, the following terminology is introduced: the `parent channel` is a channel with the name of the current channel up to and excluding the last forward slash character in the name. If no forward slash occurs in the name, the primary channel is considered the parent channel.

A new error `no-such-parent-channel` is introduced. It is an `update-failure`.

§5.3.1 is modified as follows: instead of point `1`: If the parent channel does not exist, the server responds with a `no-such-parent-channel` update and drops the request. If the parent channel exists, the update is checked for permissions by the parent channel.

§5.5.1 is modified as follows: the `channels` update is upgraded to a `channel-update` and as such contains a `channel` field. When processing the `channels` update, the server should only process channels whose names begin with the name mentioned in the `channel` field followed by a forward slash and do not contain any further forward slashes.

Specifically, an update requesting ``foo`` should list ``foo/bar``, but not ``foo/bar/baz``.

Clients that support this extension are required to implement the following special semantic: if a user uses a command that requires a channel name, and the user begins the channel name with a forward slash, the client should automatically prepend the current channel name to the specified channel name, if there is a channel that is considered "current". If no channel is explicitly current, the primary channel is considered current.

If the client also supports the `shirakumo-emotes` extension, it should make sure that emotes from ancestor channels are available in any descendant channel.

#### 7.6 Channel Info (shirakumo-channel-info)
Purpose: allows associating metadata with channels such as the set of rules, a topic, and so forth.

Channels receive extra metadata fields that can be set set by users. To this end, channels must keep a table of `metadata` to track. The server must restrict the valid keys in that table, and may restrict the content of values associated with each key. The following keys must always be available, with the specified intended purposes:

- `:news` Updates and latest news by channel administrators
- `:topic` A description of the general discussion topic of the channel
- `:rules` A Description of the rules that need to be followed by channel members
- `:contact` Information on how to reach contact persons for administrative problems
- `:url` Some kind of URL to a website representing the channel

A new update called `channel-info` is introduced. It is a `channel-update` and holds a `keys` field that can either be `T` or a list of keys as symbols describing the info to fetch.

A new update called `set-channel-info` is introduced. It is a `channel-update` and a `text-update`, and holds a `key` field that must be a symbol describing the info to set.

A new error `no-such-channel-info` is introduced. It is an `update-failure` and contains the additional field `key`, which must hold a symbol.

A new error `malformed-channel-info` is introduced. It is an `update-failure`.

When the server receives a `channel-info` update, it must react as follows:

1. For each of the requested keys, the server reacts as follows:
  1. If the key does not exist, the server replies with a `no-such-channel-info` failure with the according `key` set, and the `id` set to the `id` of the original update.
  1. Otherwise, the server replies with a `set-channel-info` update with the same `id` as the request, `key` set to the current key being requested, and `text` being set to the key's value.

When the server receives a `set-channel-info` update, it must react as follows:

1. If the specified `key` is not accepted by the server, it replies with a `no-such-channel-info` error and drops the update.
1. If the specified `text` is not of the correct format for the given `key`, it replies with a `malformed-channel-info` error and drops the update.
1. The internal channel metadata is updated to associate the given `key` with the given `text`.
1. The user's `set-channel-info` update is distributed to all users in the channel.

#### 7.7 Server Management (shirakumo-server-management)
Purpose: adds capabilities for administrative actions like deleting users and channels.

The server now holds an additional property, a `blacklist`, which is a set of usernames.

§4.1 is modified to include the following step after §4.1.4 (Checking for name validity):

1. If the name is part of the `blacklist` set, a `too-many-connections` update is returned and the connection is closed.

A new update called `kill` is introduced. It is a `target-update`. When the server receives a `kill` update, it must react as follows:

1. If there is no user with a name corresponding to the `target`, the server replies with a `no-such-user` error and drops the update.
1. The user is removed from all channels it is in.
1. All connections the user is associated with are disconnected.
1. The update is sent back to the user.

A new update called `destroy` is introduced. It is a `channel-update`. When the server receives a `destroy` update, it must react as follows:

1. Unlike standard updates, the permission for the update must be checked against the primary channel, rather than the channel the update is targeting.
1. If there is no channel with a name corresponding to the `channel`, the server replies with a `no-such-channel` error and drops the update.
1. A `leave` update for the channel is sent to every user in the channel.
1. The channel is removed.
1. The update is sent back to the user.

A new update called `ban` is introduced. It is a `target-update`. When the server receives a `ban` update, it must react as follows:

1. The `target` is added to the `blacklist` set.
1. If a user with the `target` name is present, the user is removed from all channels it is in.
1. All connections the user is associated with are disconnected.
1. The update is sent back to the user.

A new update called `unban` is introduced. It is a `target-update`. When the server receives an `unban` update, it must react as follows:

1. The `target` is removed from the `blacklist` set.
1. The update is sent back to the user.

A new update called `blacklist` is introduced. When a server receives a `blacklist` update, it must react as follows:

1. The update's `target` field is set to the list of usernames contained in the `blacklist`.
1. The update is sent back to the user.

#### 7.8 Pause (shirakumo-pause)
Purpose: allows throttling high traffic channels to prevent frequent spam by users.

Channels have a new property, a "pause", as well as a "last update list". Delivery of any `channel-update` is modified as follows:

1. If the `from` field of the update denotes a user whose entry in the "last update list" is a timestamp that's closer to the current timestamp than the channel's "pause", the server responds with a `too-many-updates` error and drops the update.
1. The current timestamp is placed into the user's entry in the "last update list".

The "pause" is noted in seconds and has a default value of 0. The "last update list" has a default timestamp of 0 for users without an entry.

§5.4.1 (joining of a channel) is modified by adding the following extra step at the end:

1. If the channel's "pause" property is greater than zero, a `pause` update is sent to the user with the `by` field set to the "pause" time.

A new update called `pause` is introduced. It is a `channel-update` and has an additional field called `by`, which must contain an integer in the range [0,infinity[. When the server receives a `pause` update, it must react as follows:

1. The channel's "pause" is set to "by".
1. The update is distributed to all users in the channel.

#### 7.9 Quiet (shirakumo-quiet)
Purpose: allows placing users onto a quiet list that prevents them from reaching any other users.

Channels have a new property, a "quiet list". Delivery of any `channel-update` is modified as follows:

1. If the `from` field of the update denotes a user that is on the "quiet list", the update is sent back to that user, but not delivered to the rest of the channel.
1. Otherwise delivery proceeds as normal.

A new update called `quiet` is introduced. It is a `target-update` and a `channel-update`. When the server receives a `quiet` update, it must react as follows:

1. The target user is placed onto the "quiet list" of the channel.
1. The update is sent back to the user.

A new update called `unquiet` is introduced. It is a `target-update` and a `channel-update`. When the server receives an `unquiet` update, it must react as follows:

1. The target user is removed from the "quiet list" of the channel.
1. The update is sent back to the user.

A new update called `quieted` is introduced. It is a `channel-update`. When the server receives a `quieted` update, it must react as follows:

1. The update's `target` field is filled with a list of usernames on the channel's quiet list.
1. The update is sent back to the user.

#### 7.10 IP (shirakumo-ip)
Purpose: exposes IP address information and allows management of IPs.

Connections now have an additional property, the `ip`, which must be an IPv6 address. If the connection is established over IPv4, the `ip` should nevertheless be the IPv6 representation of this address.

The server has an additional property, an `ip-blacklist`, which is a set of IP addresses and masks as described below.

§4.1 is modified to include the following step before all others:

1. If the IP address the connection is coming from matches one from the `ip-blacklist`, the connection is immediately dropped.

An IP address `a` is considered the "same" as an IP address `b` under the mask `m`, if the bitwise AND of `a` and `b` with the bitwise inversion of `m` equals the same (`a & !m == b & !m`). The purpose of the mask is to allow addressing entire subnets.

A new update called `ip-ban` is introduced. It holds the required field `ip` and the optional field `mask`. If `mask` is not given, it should be assumed to be an IP address that is all 1s. Both the `ip` and `mask` field must be strings in either IPv4 or IPv6 format. When the server receives an `ip-ban` update, it must react as follows:

1. If either `ip` or `mask` do not designate IPv4 or IPv6 addresses, a `bad-ip-format` failure is sent and the update is dropped.
1. Scan through the existing `ip-blacklist` and for each:
   1. if the IP matches `ip` under `mask`:
      1. if the mask is greater (thus more general) than `mask`, the update is dropped
      1. otherwise the entry is removed.
1. The entry of `ip` and `mask` is added to the blacklist.
1. Any connection matching the new entry is dropped.
1. The update is sent back to the user.

A new update called `ip-unban` is introduced. It holds the same fields as `ip-ban`. When the server receives an `ip-unban` update, it must react as follows:

1. Scan through the existing `ip-blacklist` and for each:
   1. if the IP matches `ip` under `mask`:
      1. if the mask is greater than or equal to `mask`, the entry is removed.
1. The update is sent back to the user.

A new connection attribute called `shirakumo:ip` is introduced, which is a string showing the IP address from which the connection originates.

A new update called `ip-blacklist` is introduced. When a server receives an `ip-blacklist` update, it must react as follows:

1. The update's `target` field is set to the list of IP addresses and masks contained in the `ip-blacklist`.
1. The update is sent back to the user.

#### 7.11 Bridge (shirakumo-bridge)
Purpose: allows bridging chat channels from external services by sending messages on behalf of other users.

A new, optional field `:bridge` is added to all `channel-update`s. Handling of `channel-update`s is modified as follows, after the check of §5.1.6 (`channel` existence check):

1. If the `bridge` field is set:
   1. If the user is not in the channel, a `not-in-channel` failure is sent back and the update is dropped.
   1. If the user does not have the permission to send a `bridge` update in the channel, an `insufficient-permissions` failure is sent back and the update is dropped.
   1. The values of the `bridge` and `from` fields are swapped.
   1. If the update would be delivered to all members of the channel ignoring all validity checks (it is not an update made for side-effects), then the update is sent to all members of the channel.
   1. The update is dropped.

The server may choose to discard the `bridge` field for any number of update types, but must in the very least support `message`.

A new update called `bridge` is introduced. It is a `channel-update`. When the server receives a `bridge` update, it must react as follows:

1. The update is sent back to the user.

#### 7.12 Link (shirakumo-link)
Purpose: allows storing data server-side to deliver it in a more efficient out-of-band fashion.

Clients and servers implementing this extension must also implement the `shirakumo-data` extension.

A new field called `shirakumo:link` is added to the `message` update.

When the server receives a `data` update, it must instead of §7.2.3 (distributing the update) act as follows:

1. The data payload is saved somewhere, such that it is publicly accessible through an HTTP or HTTPS URL.
1. A new `message` update is generated from the `data` update, with the `text` being the public URL of the payload, and the `shirakumo:link` attribute containing the content-type.
1. The new `message` update is distributed to the channel.
1. The update is dropped

The server may opt to make saved data payloads inaccessible after a time. The server should take care to generate URLs for the data payloads that are not guessable, which is to say a user cannot reliably generate URLs to access a payload. The server must serve the payload with the requested `content-type` set, and the `content-disposition` header may be set to include the requested filename in the update. The server must delete all data payloads if a channel is deleted (due to expiration or otherwise). The server may merge payloads that designate the same (byte-identical) files to the same URL.

When the client receives a `link` update, it must, as far as possible, embed the linked payload to display it directly. If it cannot display the payload directly, it may instead display the URL to which the link points.

#### 7.13 Markup (shirakumo-markup)
Purpose: allows using different markup languages to stylise the text in `text-update`s.

`text-update`s all receive the following additional fields:

- `rich` A `string` containing a version of the text with markup information included. The `text` field must be set to the same text as this field, but with all markup information stripped away.
- `markup` A `string` identifying the markup that is used in the rich text.

When a client receives a `text-update` it should check the `markup` field and determine whether it supports rendering the given format. If it is supported, the client should render the text according to the `rich` contents instead of the `text` contents. The client may ignore parts of the rich text if it considers the markup unsuitable. If the markup contains errors, the client must fall back to displaying the unformatted `text` instead.

The following markup formats are officially recognised, though others may be used:

- `html` HTML5 content
- `markless` [Markless](https://shirakumo.github.io/markless)
- `markdown` [Markdown](https://daringfireball.net/projects/markdown/syntax)
- `org` [org-mode](https://orgmode.org)
- `rest` [reStructuredText](https://docutils.readthedocs.io/en/sphinx-docs/user/rst/quickstart.html)

#### 7.14 User Info (shirakumo-user-info)
Purpose: allows associating additional information with registered user accounts.

Profiles receive extra metadata fields that can be set set by users. To this end, profiles must keep a table of `metadata` to track. The server must restrict the valid keys in that table, and may restrict the content of values associated with each key. The following keys must always be available, with the specified intended purposes:

- `:birthday` A textual description of the user's birthday.
- `:contact` Other contact methods, typically email addresses.
- `:location` A textual description of the user's real-world location.
- `:public-key` A PGP public key.
- `:real-name` The user's real-life name.
- `:status` May be `"away"`, or some arbitrary status description.

The `user-info` update is changed to now hold an optional `info` field that is an association list.

A new update called `set-user-info` is introduced. It is a `text-update`. It holds a `key` field that must be a symbol describing the info to set.

A new error `no-such-user-info` is introduced. It is an `update-failure` and contains the additional field `key`, which must hold a symbol.

A new error `malformed-user-info` is introduced. It is an `update-failure`.

When the server receives a `user-info` update, it must react as follows, in addition to the standard behaviour described in §5.5.3:

1. If the target user is not registered, this section is ignored.
1. For each user info key on the user's profile:
   1. A list composed of the key and the value of the field are added to the `info` field of the `user-info` reply.

When the server receives a `set-user-info` update, it must react as follows:

1. If the target user is not registered, the server replies with a `no-such-profile` failure and drops the update.
1. If the specified `key` is not accepted by the server, it replies with a `no-such-user-info` error and drops the update.
1. If the specified `text` is not of the correct format for the given `key`, it replies with a `malformed-user-info` error and drops the update.
1. The internal user metadata is updated to associate the given `key` with the given `text`.
1. The `set-user-info` update is sent back.

#### 7.15 Shared Identity (shirakumo-shared-identity)
Purpose: allows creating tokens that let other users post updates on behalf of another (registered) user account.

User profiles now have an additional field, a "lending map", which is a map associating keys (strings of at least 16 characters in length) to other usernames, and an "identities list", which is a list of usernames they can send updates on behalf of.

§5.1.5 (`from` field check) is modified as follows:

1. If the `from` field is in the connection's associated profile's "identities list" the `from` field check is elided.

A new update called `share-identity` is introduced. When the server receives a `share-identity` update, it must react as follows:

1. If the user is not registered, a `no-such-profile` failure is sent back and the update is dropped.
1. If the profile already has too many identity shares, an `identity-already-used` failure is sent back and the update is dropped.
1. A new random key is generated and associated with `nil` in the profile's lending map.
1. The update is sent back with the `key` field set to the newly generated key.

A new update called `unshare-identity` is introduced. When the server receives an `unshare-identity` update, it must react as follows:

1. If the user is not registered, a `no-such-profile` failure is sent back and the update is dropped.
1. If the `key` is not set, the profile's lending map is emptied and the user's name is removed from all identities lists.
1. Otherwise, the entry corresponding to the `key` is removed from the profile's lending map, and the user's name is removed from the identities list of the user who redeemed the key.
1. The update is sent back.

A new update called `list-shared-identities` is introduced. When the server receives a `list-shared-identities` update, it must react as follows:

1. If the user is not registered, a `no-such-profile` failure is sent back and the update is dropped.
1. For every entry in the profile's lending map, the server gather's the key, as well as the username the associated connection is from (or `nil` if the key is not associated yet) into a list as for example:
   ```
   (("aoeubcoeusasoet425" "test") ("aoestuhau245757Saoeus" NIL))
   ```
1. The update is sent back, with the `shares` field set to the gathered list, and the `identities` field set to the user's identities list.

A new update called `assume-identity` is introduced. It is a `target-update`. When the server receives an `assume-identity` update, it must react as follows:

1. If the user is not registered, a `no-such-profile` failure is sent back and the update is dropped.
1. If the target is already on the identities list of the profile, or the user is the target, an `identity-already-used` failure is sent back and the update is dropped.
1. If the `key` in the update is either not in the target profile's lending map, or the key is not associated with `nil`, an `identity-already-used` failure is sent back and the update is dropped.
1. The originating user is associated with the `key` in the target profile's map of shares, and the `target` is added to the originating profile's identities list.
1. The update is sent back to the originating connection.

#### 7.16 Icons (shirakumo-icons)
Purpose: allows associating icons with channels and users.

This extension requires the `shirakumo-channel-info` or `shirakumo-user-info` extensions.

For channels and users, a new key type is introduced:

- `:icon` The value of which must be a base64 encoded image file, with the content type prefixed like so: `content-type base64`.

The server may reject images that are too large in dimension, or have a bad content-type. The server must in the very least support `image/png` and `image/gif` as content-types.

#### 7.17 Signing (shirakumo-sign)
Purpose: allows users to sign their messages with a PGP signature to ensure authenticity of the message.

`update`s now have an additional field, `signature`. The signature is computed based on the following `utf-8` representation of updates:

```
UPDATE   ::= OBJECT NULL
OBJECT   ::= '(' SYMBOL (SPACE KEYWORD SPACE EXPR)* ')'
EXPR     ::= STRING | LIST | SYMBOL | NUMBER
STRING   ::= '"' ('\' '"' | '\' '\' | !('"' | NULL))* '"'
LIST     ::= '(' (EXPR (SPACE EXPR)*)? ')'
SYMBOL   ::= KEYWORD | NAME | NAME ':' NAME
KEYWORD  ::= ':' NAME
NUMBER   ::= '0..9'+ ( '.' '0..9'*)?
NAME     ::= ('\' '\' | '\' TERMINAL | !(TERMINAL | NULL))+
TERMINAL ::= (':' | ' ' | '"' | '.' | '(' | ')')
SPACE    ::= U+0020
NULL     ::= U+0000
```

This is equivalent to the standard wire format structure and the recommended way of printing, but enforces single space between tokens, no use of backslash escapes unless necessary, and forces a leading digit on numbers, essentially eliminating all ambiguities in the syntax. Additional constraints on printing the update's fields apply:

- The fields must be printed by sorting them according to their unicode codepoints, meaning order is determined by comparing each codepoint in two candidates in turn and sorting the lower codepoint to be before the higher codepoint.
- The `signature` field must be omitted.
- The `clock` and `from` fields must be present.

As an example, printing a standard `message` update would look as follows:

```
(message :channel "test" :clock 424742 :id 0 :from "tester" :text "something")
```

When the server receives a message with the `signature` field set, the following constraints apply to §5.1:

- If the `from` or `clock` fields are not set, a `malformed-update` update is sent back and the request is dropped.
- The server must not adjust the `clock` value.

After the signature has been computed from the printed representation, the client should attach it to the `signature` field of the update and send it to the server. Clients may then verify the signature using the PGP public key of the user, if known. It is recommended for clients that support this extension to give a visual indicator for signed updates, especially if the signature verification should fail.

A user's pgp key may be retrieved out of band, or using the `shirakumo-user-info` `:public-key` field if available.

#### 7.18 History (shirakumo-history)
Purpose: allows users to search through the history of a channel to find relevant messages.

In order to facilitate this, the server must now keep updates in storage, potentially indefinitely. The server is only required to keep updates of type `message`, but may keep other updates of type `channel-update` as well. Of each update stored, the server must store at least the fields `id`, `from`, `clock`, and `channel`. It may store additional fields, and it may also drop them. This means that the server is not required to fully keep update identity.

A new update type called `search` is introduced. It is a `channel-update`, and holds three additional fields, `results`, `offset`, and `query`. The `query` field must hold a list of initargs, meaning alternating symbols and values to describe the keys to match. When the server receives a `search` update, it must proceed as follows:

1. If the user is not in the named channel, a `not-in-channel` update is sent back and the request is dropped.
1. It gathers a list of all recorded updates that were posted to the channel specified in the `search` update.
1. For each update in the list:
   1. The `query` field is compared against the update by comparing each initarg in the query to the corresponding field in the update, based on the field's type:
      - `time`: The query value must be a list of two elements, being either `T` or a `time` value, with `T` being "any time". The first element designates the lowest possible time to match, and the second the highest possible time to match. This thus designates a range of time values, with the bounds being inclusive.
      - `number`: The query value must be a number, and is compared to the candidate value by a standard number equality test.
      - `symbol`: The query value must be a symbol, and matches if the candidate is the same symbol.
      - `list`: The query value must be another list, and matches the candidate if all of the elements of the query appear in the candidate list, regardless of order. Elements are compared recursively.
      - `string`: The query value must be a list of strings, each of them designating a matching spec:
        ```
        MATCH        ::= CHAR*
        CHAR         ::= ESCAPED | ANY | NONE-OR-MORE | character
        ESCAPED      ::= '\' character
        ANY          ::= '_'
        NONE-OR-MORE ::= '*'
        ```
        Where `ANY` stands for any particular character, and `NONE-OR-MORE` stands for an arbitrary number of arbitrary characters. When matching a single "character" with `ANY`, the Unicode Collation Algorithm rules must be followed. The query value matches if at least one of its strings matches.
   1. Should any of the fields not match, the update is removed from the list.
1. The list of updates is sorted in order of their `clock` with the lowest being first.
1. The first N updates are dropped off the list, where N corresponds to number in the `offset` field. If `offset` is not specified, 0 is assumed.
1. The first N updates are kept and the rest dropped off the list, where N is an internal server limit, which must be at least 50.
1. The list of updates is split into multiple lists such that each list can be reliably sent back to the user.
1. For each list of updates, the list is put into the `search` update's `results` field and the update is sent back.

The server should provide a means to delete updates from its history to ensure confidential and private information can be removed and is not preserved indefinitely. If the `search` update has a permission of `NIL` (being denied to everyone) in a channel, the history does not need to be recorded. If a channel expires, its history must be deleted.

The client should provide a convenient means to perform a search query. To this end we also specify a suggested means of formatting queries for end-user input. The query should be specified as freeform text, with the following queryspec format:

```
QUERY    ::= (FIELD | TOKEN)+
FIELD    ::= WORD ':' TOKEN
TOKEN    ::= STRING | WORD
STRING   ::= '"' ('\' char | !'"') '"'
WORD     ::= (!TERMINAL)+
TERMINAL ::= ':' | ' ' | '"'
```

Where a `FIELD` specifies a specific field to match in an update, with the `WORD` designating the initarg and the `TOKEN` the value. Special 'virtual fields' should be added:

- `after` Designates the former element in a `time` match for the `clock` field. If only `after` is specified, the latter element is `T`. The actual format of the `TOKEN` does not have to be an integer, but should be some human-readable datestring.
- `before` Designates the latter element in a `time` match for the `clock` field. If only `before` is specified, the former element is `T`. The actual format of the `TOKEN` does not have to be an integer, but should be some human-readable datestring.
- `in` Designates the `channel` field. If not present, the client should infer the channel to use from the current channel.

The `TOKEN` should be parsed according to the standard wire format, unless more specific and convenient ways of specifying a fitting value are available. Each `TOKEN` than is not part of a `FIELD`, should designate a matching spec to be part of the list of matching specs for the `text` field.

In other words, the following queryspec:

```
from:tester after:2020-01-01 this "that is" in:test
```
Should be translated into a search update like this:
```
(search
 :id 0
 :channel "test"
 :query (:from "tester"
         :clock (3786825600 T)
         :text ("this" "that is")))
```

The client should also offer an easy way to page through the results using the `offset` field. The end of the paging may be detected should the server ever return less than 50 results.

If the server also supports the `shirakumo-backfill` extension, it may deliver backfill using the history, even if users were not previously in the channel. This poses a privacy risk, but as search is not otherwise restricted anyway, it makes no difference.

#### 7.19 Block (shirakumo-block)
Purpose: allows users to block other users, preventing seeing their updates. Having this property server-side instead of client-side means it is automatically persisted and synchronised.

Profiles now have a new field: a block list. This is a list of usernames.

Whenever an update is distributed over a channel, the following behaviour must be followed:

1. For each (target) user in the channel:
  1. If the user noted in the `from` field of the update is *not* the block list of the target user's profile:
  1. The update is sent to all connections associated with the target user.

A new update type called `block` is introduced. It is a `target-update`. When the server receives a `block` update, it must react as follows:

1. If the user is not registered, the server replies with a `no-such-profile` failure and drops the update.
1. The username from the `target` field is added to the profile's block list if it isn't present already.
1. The update is sent back to the user.

A new update type called `unblock` is introduced. It is a `target-update`. When the server receives an `unblock` update, it must react as follows:

1. If the user is not registered, the server replies with a `no-such-profile` failure and drops the update.
1. The username from the `target` field is removed to the profile's block list.
1. The update is sent back to the user.

A new update type called `blocked` is introduced. When the server receives a `blocked` update, it must react as follows:

1. If the user is not registered, the server replies with a `no-such-profile` failure and drops the update.
1. The update's `target` field is filled with a list of usernames on the sending user profile's block list.
1. The update is sent back to the user.

#### 7.20 Reactions (shirakumo-reactions)
Purpose: allows users to react to messages without sending a new message.

This requires clients to implement unique IDs when sending an update. They do not need to be globally unique, but should be unique to that user, regardless of connection used.

A new update type called `react` is introduced. It is a `channel-update` and carries the additional fields `update-id`, `target`, and `emote`. `update-id` must be an `id` used in a message previously sent by the `target` user in the `channel`. `emote` must either be unicode characters from the emoji block, or if the `shirakumo-emote` extension is supported, the name of an emote.

When the server receives a `react` update, it must act as follows:

1. If the user is not in the named channel, a `not-in-channel` update is sent back and the request is dropped.
1. If the `emote` does not contain valid text as noted above, a `malformed-update` failure is sent back and the update is dropped.
1. The user's `react` update is distributed to all users in the channel.

For each visible update the client should now keep track of a table of reactions from emotes to lists of users that used the emote on the referenced update. When the client receives a `react` update, it should update the table as follows:

1. If the emote is not in the table yet, a new entry is added, associated to an empty list.
1. If the `from` user is not in the list of users, the user is added to it. Otherwise the user is removed from it.
1. The table of emotes is displayed in the vicinity of the referenced update as a list of emotes and counts of users having used said emote. If applicable, the list of users having used the emote should also be displayed via a hover action.

If a `react` update references an update that is not known to the client, it is ignored.

#### 7.21 Replies (shirakumo-replies)
Purpose: allows specifying another message that a message is in reply to.

This requires clients to implement unique IDs when sending an update. They do not need to be globally unique, but should be unique to that user, regardless of connection used.

A new field is added to the `message` update: `reply-to`. The field should hold a list of two values, a username and an id, identifying the message this message is in reply to.

The server does not have to do anything special aside from transmitting the field.

When the client receives a `message` update with the `reply-to` field set, it should display the message in relation to the original message, by quoting it or linking back to it in some manner.

#### 7.22 Last Read (shirakumo-last-read)
Purpose: allows tracking which message in a channel was last read across connections.

This requires the server to track a new per-user property in each channel: 'last read message', which is a tuple of a message ID and a username.

A new update is introduced called `last-read`, which is a `channel-update`. It has two additional optional fields, `update-id`, and `target`. `update-id` must be an `id` used in a message previously sent by the `target` user in the `channel`.

When the server receives a `last-read` update, it proceeds as follows:

1. If the `update-id` and `target` fields are set:
  1. The id and target are stored for the user's 'last read message' in the associated channel.
  1. The update is sent back to the user.
1. Otherwise:
  1. The update is modified to use the 'last read message' information to fill in the `update-id` and `target` fields.
  1. The update is sent back to the sending connection.

If the user leaves a channel, the 'last read message' tuple may be unset. If a user enters a channel, the 'last read message' tuple must be set to the `enter` message's `id` and `from` fields, should the 'last read message' tuple be unset.

#### 7.23 Typing (shirakumo-typing)
Purpose: allows specifying when a user is in the process of typing a message.

A new update is introduced called `typing`, which is a `channel-update`.

When the server receives a `typing` update, it proceeds as follows:

1. The update is distributed to all users in the channel.

When a client receives a `typing` update, it should notify the user that the update's sender is typing something. If no new `typing` update is received within the next 5 seconds, the typing notification should be cleared.

When the client's user types, the client may send a `typing` update to the current channel, as long as the previous `typing` update was sent more than 4 seconds ago.

### 8 General Conventions
The following are general conventions for server and client implementors. However, they are not mandatory to follow, as they may only make sense for certain types of implementations.

The default port when served over TCP should be `1111`, with `1112` being the default for SSL connections.

When specified in URLs, Lichat takes the `lichat` protocol and follows this general scheme: `lichat://host:port/channel#id` Meaning the URL path (if given) is used as the name for a channel to join. The URL fragment can be used to specify a specific message id. Note that it is the client's responsibility to ensure that the ID is sufficiently unique so that the URL will link to the correct message. The query part of the URL may be used for client-specific purposes.

## See Also

* [lichat-serverlib](https://shirakumo.github.io/lichat-serverlib) An agnostic implementation of the server-side protocol.
* [lichat-tcp-server](https://shirakumo.github.io/lichat-tcp-server) A basic, threaded, TCP-based implementation of a Lichat server.
* [lichat-tcp-client](https://shirakumo.github.io/lichat-tcp-client) A basic, threaded, TCP-based implementation of a Lichat client.
* [LionChat](https://github.com/Shirakumo/lionchat) A Qt GUI client for a TCP server.
* [Ocelot](https://github.com/Shirakumo/ocelot) An Android client for Lichat.
* [ex-lichat](https://github.com/Shirakumo/ex-lichat) An Elixir server for Lichat.
