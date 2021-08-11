ForgeFed/ActivityPub Federation in Vervis
=========================================

At the time of writing, here's the current status of federation implemented in
Vervis.

Summary:

* To post a comment on a local ticket (same server), log in and browse
  to the ticket's page and use the ticket reply form, the regular way it's been
  on Vervis.
* To post a comment on a remote ticket (other server), log in and browse to the
  /publish page, fill the form and the comment will be delivered to the right
  place

For more details, read below.

## Federation triggered by regular UI

* Ticket comments are federated. If you submit a ticket comment, local and
  remote users who previously commented on the same ticket will get your
  comment delivered to their inboxes. The user who created the ticket's project
  will have it delivered to them too.
* If you comment on a ticket, you automatically become a ticket follower, and
  all future comments on the ticket will be delivered to your inbox.
* You can see users' outboxes.
* You can see your inbox.
* If you create a project, all comments on all tickets of the project will be
  delivered to your inbox.
* There is UI for notifications about comments on tickets you commented on or
  whose projects you created. However there's no JS to display them in
  real-time and no email integration.

The ticket comment UI allows to see tickets and comments, and if you're logged
in, you can post new comments. If you wish to post a comment on a ticket hosted
on another server, not the one on which your account is hosted, see the
dedicated federation pages listed below.

## GET endpoints

`GET /publish`

A page where you can write and publish a ticket comment, either on a local
ticket (i.e. a ticket on a project hosted on the same server as your account)
or on a remote ticket (i.e. a ticket on a project hosted on some other server).

`GET /inbox`

A test page that displays received activities and the result of their
processing.

`GET /s/joe/inbox`

A page that displays your personal inbox. It should list all ticket comments on
projects you've created and and ticket comments on tickets you previously
commented on.

`GET /s/joe/outbox`

A page that displays your personal outbox. It should list all the activities
you're published, all ticket comments you've made.

## POST endpoints

`POST /s/joe/outbox`

Personal endpoint for publishing ticket comments. When you submit the form in
the /publish page, this is where it is sent. In the future you'll be able to
see the content of your outbox, and other people will be able to see the public
items in your outbox.

You can access this endpoint without using the /publish page, but Vervis
doesn't have OAuth2 support yet, so you'll need to log in first and grab the
cookie, and send it along with the request.

`POST /s/joe/inbox`

Personal endpoint to which other servers deliver ticket comments for you to
see. These are comments on tickets on which you previously commented, and thus
automatically became a follower of thosr tickets.

`POST /s/joe/p/proj/inbox`

Per-project inbox, to which projects receive ticket comments from other
servers. If someone on another server publishes a comment on your project, then
your project will receive the comment at this endpoint and the comment will be
displayed when you visit the ticket page.

## Spec

Federation in Vervis is done using ActivityPub. Below comes a description of
the details that aren't already common on the Fediverse. The details are
written informally in the form of short simple proposals.

### (A) Authentication

Vervis uses HTTP Signatures to authenticate messages received in inboxes. The
Host, (request-target), Date and Digest headers are required to be present and
used in the signature, and the Digest header must be verified by computing the
hash of the request body. Other headers may need signing too, as specified in
the proposals below.

The `publicKeyPem` field maps to the PEM encoding of the key. The PEM encoding
contains not just the key itself, but also a code specifying the key type. The
Fediverse de-facto standard is RSA, more precisely PKCS#1 v1.5, and used with
the SHA-256 hash algorithm. This is often referred to as RSA-SHA256.

#### (1) Actor key(s) in a separate document

Allow an actor's signing key to be a separate document, rather than embedded in
the actor document. In Vervis, the use of that is for server-scope keys (see
proposal below), but otherwise, an embedded key is just as good.

`GET /users/aviva/keys/key1`

```json
{ "@context":     "https://w3id.org/security/v1"
, "@id":          "https://example.dev/users/aviva/keys/key1"
, "@type":        "Key"
, "owner":        "https://example.dev/users/aviva"
, "publicKeyPem": "-----BEGIN PUBLIC KEY----- ..."
}
```

`GET /users/aviva`

```json
{ "@context":
    [ "https://www.w3.org/ns/activitystreams"
    , "https://w3id.org/security/v1"
    ]
, "id":                "https://example.dev/users/aviva"
, "type":              "Person"
, "preferredUsername": "aviva"
, "name":              "Aviva"
, "inbox":             "https://example.dev/users/aviva/inbox"
, "outbox":            "https://example.dev/users/aviva/outbox"
, "publicKey":         "https://example.dev/users/aviva/keys/key1"
}
```

Authentication requirements:

- The `keyId` from the signature header matches the `@id` in the document you
  receive
- The and key and the owner actor IDs are on the same host
- They key specifies the `owner`, and the owner actor's `publicKey` links back
  to the key

#### (2) Multiple actor keys

Allow an actor to specify more than one key, or no key at all. This means that
when you examine the owner actor of the key, you verify the actor links back to
the key by checking that the key is listed among the actor's keys (instead of
requiring/expecting only a single key to be specified by the actor).

The reason this is used in Vervis is for key rotation using a pair of
server-cope keys (see proposal below).

When used along with proposal A.1, each key may be either embedded in the
document, or a URI specifying the ID of a key defined in a separate document.

Actors that never need to post activities can simply not specify any keys at
all.

`GET /users/aviva`

```json
{ "@context":
    [ "https://www.w3.org/ns/activitystreams"
    , "https://w3id.org/security/v1"
    ]
, "id":                "https://example.dev/users/aviva"
, "type":              "Person"
, "preferredUsername": "aviva"
, "name":              "Aviva"
, "inbox":             "https://example.dev/users/aviva/inbox"
, "outbox":            "https://example.dev/users/aviva/outbox"
, "publicKey":
    [ { "id":           "https://example.dev/users/aviva#main-key"
      , "type":         "Key"
      , "owner":        "https://example.dev/users/aviva"
      , "publicKeyPem": "-----BEGIN PUBLIC KEY----- ..."
      }
    , "https://example.dev/users/aviva/extra-keys/extra-key1"
    , "https://example.dev/users/aviva/extra-keys/extra-key2"
    ]
}
```

#### (3) Server-scope actor key

Allows to have actor keys that can be used to sign (and verify) activities of
any actor on the server, not limited to any specific actor. That allows to have
some small constant number of keys on the server, which is very easy to manage
and makes key rotations very cheap. It also saves storage of many local and
remote actor keys.

In the common Fediverse situation, there's a separate key for each actor, but
all of these actor keys are managed by a single entity, the server. The
signatures aren't made on users' devices using private keys they keep to
themselves. They're made by the server, using private keys the server
generates.

Server-scope keys are made by the server too. The server makes the signatures,
using a private key it generates and maintains. The server is the owner of the
key, and a part of the signed message is the ID of the actor on whose behalf
the message is being sent. Since the actor isn't specified by the key, the
actor ID is instead placed in a HTTP header. And the actor still has to list
the key under `publicKey` as usual.

`GET /key1`

```json
{ "@context":
    [ "https://w3id.org/security/v1"
    , { "isShared": "https://angeley.es/as2-ext#isShared"
      }
    ]
, "@id":          "https://example.dev/key1"
, "@type":        "Key"
, "owner":        "https://example.dev"
, "isShared":     true
, "publicKeyPem": "-----BEGIN PUBLIC KEY----- ..."
}
```

`GET /users/aviva`

```json
{ "@context":
    [ "https://www.w3.org/ns/activitystreams"
    , "https://w3id.org/security/v1"
    ]
, "id":                "https://example.dev/users/aviva"
, "type":              "Person"
, "preferredUsername": "aviva"
, "name":              "Aviva"
, "inbox":             "https://example.dev/users/aviva/inbox"
, "outbox":            "https://example.dev/users/aviva/outbox"
, "publicKey":
    [ { "id":           "https://example.dev/users/aviva#main-key"
      , "type":         "Key"
      , "owner":        "https://example.dev/users/aviva"
      , "publicKeyPem": "-----BEGIN PUBLIC KEY----- ..."
      }
    , "https://example.dev/users/aviva/extra-keys/extra-key1"
    , "https://example.dev/users/aviva/extra-keys/extra-key2"
    , "https://example.dev/key1"
    ]
}
```

Requirements for a server-scope key:

- Its `owner` is the top-level URI of the server, of the form `https://HOST`
- The `isShared` property is `true`
- The key is in its own document, not embedded in an actor

Requirements for authentication using a server-scope key:

- The actor ID is specified in the `ActivityPub-Actor` HTTP header
- The actor and key are on the same server
- That header is included in the HTTP Signature in the `Signature` header
- That actor lists the key (as one of the keys) under `publicKey`
- In the payload, i.e. the activity in the request body, the activity's actor
  is the same one specified in the `ActivityPub-Actor` (unless the activity is
  forwarded, see proposal B.2 about inbox forwarding)

#### (4) Actor key expiration and revocation

Allow to improve the secure handling of signing keys by supporting expiration
and revocation. Expiration means the key specifies a time at which it stops
being valid, and once that time comes, signatures made by that key are
considered invalid. Revocation similary means the key specifies a time at which
it stops being valid.

`GET /users/aviva/keys/key1`

```json
{ "@context":     "https://w3id.org/security/v1"
, "@id":          "https://example.dev/users/aviva/keys/key1"
, "@type":        "Key"
, "owner":        "https://example.dev/users/aviva"
, "created":      "2019-01-13T11:00:00+0000"
, "expires":      "2021-01-13T11:00:00+0000"
, "publicKeyPem": "-----BEGIN PUBLIC KEY----- ..."
}
```

Requirement: When verifying a signature, compare `expires` and `revoked`, if
one of them or both of them are present, to the current time. If at least one
of the 2 times is the current time or earlier, then consider the signature
invalid. If using a cached version of the key, try to HTTP GET the key and try
to authenticate once more, because it's possible the key has been replaced with
a new valid one.

#### (5) Ed25519 actor keys

Allows actor keys to be [Ed25519](https://ed25519.cr.yp.to) keys, by allowing
the `publicKeyPem` field to simply contain a PEM encoded Ed25519 public key.
The [HTTP Signatures draft](https://tools.ietf.org/html/draft-cavage-http-signatures-11#appendix-E.2)
lists more algorithms; we could support them too. This proposal just suggests
that we all start supporting Ed25519 in addition to RSA.

#### (6) HTTP Signature draft 11

The draft linked above, from April 2019, makes some changes and
recommendations. This proposal suggests we adopt them:

- For the `algorithm` parameter, use the value `hs2019`, or none, and start
  deprecating the old values (such as `rsa-sha256`).
- The new `created` and `expires` parameters seem to be mostly useful to web
  browser based clients, while our usage of HTTP Signatures is between servers.
  So perhaps they aren't very useful here. But if someone finds them useful,
  let's support them.
- Support at least Ed25519 in addition to RSA, see proposal A.5 above.

#### (7) Key rotation using a pair of server-scope keys

Allows to easily and computationally-cheaply perform periodic key rotation.

Rationale:

If you deliver an activity and then rotate the key, the target servers will
want to fetch the old key to verify your signatures, but, the old key has been
replaced, so they will fail to authenticate your requests. When using per-actor
keys, it's possible to try waiting for a time the user is inactive (which is
hopefully common because most people probably sleep for a few hours every day),
and use that as a safer chance to rotate the key. During the quiet time, other
servers will have had enough time to process their activity inbox queues, and
by the time we rotate, nobody will want the old key anymore.

The weakness of that solution is that:

- It's limited to periods of inactivity, which may limit rotation to once per
  day or less (what if you want to rotate more often? Hmm is there a good
  reason to? I'm not sure, just saying hypothetically)
- It doesn't work for users that don't have inactivity periods, e.g. a user
  that uses scheduled activities, automatic responses etc.
- It involves the computation of generating a new key for every user every day
  (assuming we don't want to rotate more often), which I suppose can be
  somewhat heavy, especially for RSA (but I haven't done any measurements)
- It involves lots of network activity because other servers will be fetching
  the new rotated keys all the time, keys can't be cached for days or weeks or
  more if they keep being replaced every day or every hour (but I haven't done
  measurements of the effect on the amount of network requests)

The proposal:

- Each server has 2 or more server-scope keys. For simplicity of discussion,
  let's assume a server has exactly 2 keys, key A and key B.
- The server does periodic rotation, but each time, it rotates one of the keys
  and leaves the other intact. It rotates key A, then next time it rotates key
  B, next time it rotates key A again, next time it rotates key B again... and
  so on.
- When signing HTTP requests, the server always uses the newer key. For
  example, if it just rotated key A, it will sign the next requests with key A.
  When time comes for the next rotation, it will rotate key B and stop using
  key A, switching to using key B for signing requests.
- The time frame suggested here for letting other servers finish processing our
  activities in their inbox queues is **one hour**, although this is just a
  suggestion and open to discussion. So it's suggested you do periodic rotation
  at most once an hour (or at least leave a key available for at least an hour
  without change after you stop using it)

That way, when one of the keys is rotated, the other key is still available for
another hour and other servers are able to use it to verify the signatures we
sent. There's no need to wait for users to be inactive, and it's very cheap:
Rotate 1 key per hour. Especially if that key is Ed25519.

### (B) ActivityPub

The following proposals are federation features in Vervis, or plans and ideas
not implemented yet, but they aren't specific to forges, and other kinds of
servers can benefit from them just as much.

#### (1) Non-actor audience

In C2S, the client sends the server an activity (or an object to be wrapped in
a `Create`) that includes addressing properties, and the server delivers the
activity to the listed recipients. The recipients may be listed as URIs or as
embedded objects, and the audience can be anything, any Object, according to
the AS2 vocabulary spec. Not limited to actors or collections. However, there
are 2 kinds of recipients:

- Actors: Recipients that have inboxes and you HTTP deliver the activity to
  them.
- Non-actors: Recipients to whom you don't directly HTTP deliver, but possibly
  you dereference them in some way and a list of more actors to deliver to.
  Usually these non-actor recipients are Collections, and they are resolved
  into lists of actors, and delivery to them sometimes involves inbox
  forwarding.

If the server gets just a list of URIs, some of which are on other servers, it
has to HTTP GET them, and find out that some are actors and some are
collections (or other non-actor objects). It may also do caching, remembering
remote actors and collections in its database to avoid HTTP GETing them every
time, but even then, it involves the initial GET where it fetches them and
remembers in the DB.

Observation: Very possibly, the client is *aware* which recipients are actors
and which aren't. Especially when the non-actors are collections. So, the
client can *hint* the server about that, so that the server doesn't even need
to do the initial GET for recipients already known to be non-actors.

There are 2 ways specified below, for making that hint. One is what Vervis
currently does, and the other is perhaps a better way I'd like to propose as an
alternative, and possibly switch Vervis to that better way.

What Vervis currently does is to use a custom `nonActors` property, which lists
recipients. The client uses that property to provide a list of recipients that
are known to be non-actors. For example if the `to` property is `[x,y,z]` and
the `nonActors` field is `[y]`, then the server can skip trying to GET the `y`
recipient, and attempt delivery only to `x` and `z`.

Another way, which is perhaps better and I'd like to propose, is to allow the
client to specify the type of the recipient in the addressing properties
themselves. For example, instead of this:

```json
[ "https://example.dev/users/martin"
, "https://example.dev/users/martin/followers"
]
```

We could do this:

```json
[ { "id":   "https://example.dev/users/martin"
  , "type": "Person"
  }
, { "id":   "https://example.dev/users/martin/followers"
  , "type": "Collection"
  }
]
```

The problem is:

- There's no `Actor` type in ActivityPub, and if you use some custom actor type
  that the server doesn't recognize, it will have to GET the recipient to be
  sure.
- There could be a custom type that is a subclass of `Collection`, and if the
  server doesn't recognize it, it will have to GET the recipient to be sure.

Since this is just a hint, and it's C2S where the client and server *possibly*
speak the same custom properties, these problems don't make the hint useless,
but they still make this approach inferior in achieving its goal, than the
custom `nonActors` property. The reason I propose it is that it uses object's
types and doesn't require any custom property. The reason Vervis doesn't use it
is that I don't see a clear way to *really* in practice tell actors from
non-actors. It also requires more computation and coding, to figure out which
things are subclasses of some actor type, or collection type, and if that's
required then RDF inference is required, therefore JSON-LD processing is
required. While in the `nonActors` approach, which maybe feels more like a
trick, it's as simple as computing set/list difference, which is generally
trivial to do.

#### (2) Authenticated inbox forwarding

When you receive an activity from another server, by some actor A, you want to
have some confidence that the activity was really published by actor A, and not
by someone pretending to be actor A, or just sending you spam attributed to
random people. This is done as follows:

- You verify the HTTP Signature of the request
- You verify the signing key's owner actor is the same actor to which the
  activity is attributed

However in some cases, such as in ForgeFed, an activity is delivered to you
indirectly, by someone who isn't the author. Specifically, there's a mechanism
in ActivityPub called *inbox forwarding*, in which a server receives an
activity at an inbox, and delivers it further to more actors. For example, in
ForgeFed, inbox forwarding is used to allow actors to address activities to
collections managed by other servers, and those servers dereference the
collections and forward the activity to the collection member actors.

This proposal suggests a way to authenticate such inbox-forwarded activities.

The concept is as follows: If Aviva sends Luke an activity, and she'd like him
to forward it, in the HTTP POST request to his inbox, she includes an
additional signature, in addition to the regular one. Luke uses the regular
signature to verify the sender is really Aviva. The additional signature, he
sends along when he forwards the activity, and the recipients use it to verify
that:

- The original author is really Aviva
- Aviva gave Luke explicit permission to forward the activity

In addition, the additional signature can be thought of as a *request* to
forward the activity.

The technical details:

- The additional HTTP Signature that Aviva includes in the POST request to
  Luke's inbox is placed in the `Forwarding-Signature`.
- Aviva also includes a header `ActivityPub-Forwarder`, whose value is Luke's
  ID URI.
- The `Forwarding-Signature` signature must use at least the headers `Digest`
  and `ActivityPub-Forwarder`.
- When Luke receives the activity from Aviva, he notices that the
  `ActivityPub-Forwarder` header is present, and that it's his ID URI, and that
  `Forwarding-Signature` is present too, and he takes that as a request and
  permission to perform inbox forwarding. He determines to whom to forward the
  activity using the ActivityPub inbox forwarding rules, as specified in the
  ActivityPub spec. Luke also may examine the `Forwarding-Signature`, verify
  that the signed headers are present, and perhaps also fetch Aviva's key and
  verify the signature.
- Luke verifies the regular HTTP Signature in Aviva's request, and verifies the
  `Digest` header by computing the request body hash
- When Luke forwards the activity to other actors, he uses the exact same
  request body that Aviva sent him, copying the bytes without any modification.
- Luke includes in his forwarding POST requests the `Digest` header copied from
  Aviva's request, and the `Forwarding-Actor` header copied as well, and also
  also her additional HTTP signature, except he places that signature in the
  `Forwarded-Signature` header, not in `Forwarding-Signature`. For the
  signature to be successfully verified by recipients, Luke will also need to
  copy any other headers used in the `Forwarding-Signature` that Aviva sent.
  Unless extensions to this proposal require other specific headers, the *only*
  headers used in the forwarding signature should be `Digest` and
  `ActivityPub-Forwarder`. In particular, don't use `Host` and
  `(request-target)`, because these vary per request and that will make
  authenticated forwarding impossible.
- Each recipient of Luke's forwarding POST tries to verify his HTTP Signature,
  to verify that Luke is indeed the author of the activity. However the
  recipient discovers that while Luke is the sender and his signature is valid,
  the author is actually Aviva. The recipient then notices the
  `Forwarded-Signature` header, which means this is a forwarded activity, and
  that the `ActivityPub-Forwarder` is Luke, which means Aviva gave Luke
  permission to forward this activity. The recipient then verifies the HTTP
  Signature in the `Forwarded-Signature` header, verifying Aviva is the
  original author and gave Luke permission to forward. The recipient then
  proceeds to process the activity as usual.

#### (3) Non-announced following

#### (4) Object nesting depth

#### (5) Object capability authorization tokens

Allows actors to delegate resource access to other actors, by sending them an
authorization token. There are many kinds of authorization tokens, and many of
them are good relevant candidates here, for example:

- OCAP-LD
- Macaroons
- JWT

This proposal, however, describes the current implementation in Vervis, which
uses a simple HMAC to authenticate the authorization token. Vervis on purpose
uses a minimal approach, so that it's easy to keep track of what its minimal
needs really are. It's totally possible and acceptable though, that this
proposal switches to a standard auth token format such as the ones listed
above. Until this proposal gets feedback and discussion, it describes the
minimal HMAC approach.

Aviva manages a yoga school. Luke is a new yoga teacher in the school, and
Aviva would like to give him access to open and lock all the rooms in the
school building. Aviva posts a `Delegate` activity to her server:

```json
{ "@context":
    [ "https://www.w3.org/ns/activitystreams"
    , { "ext":      "https://angeley.es/as2-ext#"
      , "Delegate": "ext:Delegate"
      , "Role":     "ext:Role"
      }
    ]
, "type":    "Delegate"
, "to":
    [ "https://meditation.space/users/luke"
    , "https://yoga.dev/school-staff"
    ]
, "target":  "https://meditation.space/users/luke"
, "context": "https://yoga.dev/places/school-building"
, "object":
    { "id":   "https://yoga.dev/roles/teacher"
    , "type": "Role"
    }
}
```

Aviva's server assigns an ID to the activity, and also attaches a cryptographic
proof. When Luke will later try to open doors in the school, the proof will be
used to validate his authorization token. The `proof` field maps to a Base64
encoding of the HMAC-SHA256 of the activity's ID, where the key used for the
HMAC is a secret key the server holds.

```json
{ "@context":
    [ "https://www.w3.org/ns/activitystreams"
    , { "ext":      "https://angeley.es/as2-ext#"
      , "Delegate": "ext:Delegate"
      , "Role":     "ext:Role"
      }
    ]
, "id":      "https://yoga.dev/users/aviva/outbox/m10d6"
, "type":    "Delegate"
, "to":
    [ "https://meditation.space/users/luke"
    , "https://yoga.dev/school-staff"
    ]
, "target":  "https://meditation.space/users/luke"
, "context": "https://yoga.dev/places/school-building"
, "object":
    { "id":   "https://yoga.dev/roles/teacher"
    , "type": "Role"
    }
, "proof":   "bDMCcPFntgpMoEG6SSFkXCBRm2K96h0ecFsbr11hFx0="
}
```

Later, when Luke wants to open a door, he publishes an activity and attaches
the `proof` field. Aviva's server then:

- Verifies the HMAC
- Finds the Delegation in the database
- Finds out that delegation gives Luke access as a teacher
- Verifies the HTTP Signature of the activity, thus verifying the sender is
  indeed Luke
- Checks that the door Luke wants to open can be opened by people holding a
  teacher role
- If all checks pass, Luke can open the door

#### (6) Managing actor

Allows an object to specify which actor manages it. For example, if you'd like
to send an `Update` activity, or some other activity that targets or modifies
some object, but that object isn't an actor, how do you know to which actor to
send it? This proposal proposes to have a dedicated property for this purpose,
independent of any domain-specific vocabulary or extension.

The current working name for this property is `managedBy`.

#### (7) Events collection

Defines a standard property to provide a collection of activities related to a
given object.

Suppose Aviva is writing a story, and publishing its chapters as ActivityPub
activities. Aviva is an actor, with an inbox and with an outbox, but the
chapters aren't actors. She publishes them using Create activities, in which
the objects are of type Chapter or something like that. So, when Aviva
publishes a chapter, it appears in her outbox.

A while later, Luke joins her story writing project, and he writes some
chapters too. When he writes a chapter, he publishes it and delivers to Aviva's
inbox.

From Aviva's point of view, her story's activities exist in 2 places:

- Some of them exist in her outbox (the ones she publishes)
- And some in her inbox (and ones Luke publishes, or any future contributor)

If we wanted to get a list of all the activities and changes to the story, how
would we do that? If the story were an actor, we could deliver everything to
its inbox, and then its inbox would reflect all the events and changes. But
since the story isn't an actor, there's no obvious place for this. We'd have to
somehow get a filtered view of Aviva's outbox and a filtered view of Aviva's
inbox for this. And the latter is especially problematic, because inboxes are
generally private.

This proposal suggests a property named `history`, which maps to an
`OrderedCollection` of the activities related to the object. That way, even
objects that aren't stand-alone and aren't actors can provide a stream of
updates.

#### (8) List only direct related objects, not a flattened tree

There are various properties that typically form a tree or graph structure when
recursively traversed. And often a client may wish to fetch the entire
hierarchy. For example, there's the AS2 `replies` property. The AS2 spec says
it should list objects that are responses. But should/can that include indirect
replies, i.e. objects that are replies to replies, or should only direct
replies be listed, i.e. `replies` is the inverse property of `inReplyTo`?

In ForgeFed there's similarly a `dependsOn` property for listing a ticket's
dependent tickets, and the question arises there too: Provide a flat list
containing the whole transitive closure of dependent tickets, i.e. dependencies
of dependencies etc., or list the direct dependencies?

I suppose to some people the answer to this question is obvious, but to me it
wasn't, so I'd like to explicitly propose an answer and follow it.

`replies`, and `dependsOn`, and similar properties, map to a collection whose
items are the *direct related objects*, not indirect transitively-related
ancestors of descendants. So, `replies` is the inverse property of `inReplyTo`,
and if object A lists object B under `replies`, then the `inReplyTo` field of
object `B` should be pointing back to `A`.

It's still possible for object `B` to have its own `replies`, of course,
forming a tree/graph of discussion (and `dependsOn` forming a graph of ticket
dependencies). Whether or not those nested objects forming a tree/graph are
provided, is a separate question. See proposal B.4.

### (C) ForgeFed

#### (1) Actors

How to decide which types of objects are actors and which aren't?

The proposal here is that the following types be actors:

- Person
- Project
- Repository
- Group/Organization/Team

And other types such as these not be actors:

- Ticket
- Merge request
- Patch
- Diff
- Discussion thread

The lists above are just an example of the proposed rule for determining which
objects should be actors and which not. It's not necessarily always obvious,
but the proposed guideline is:

- If the object needs to be able to publish activities, it should be an actor
- If the object is stand-alone and its meaning is self-contained, it should be
  an actor
- If the object's meaning and context are semantically inherently tied to some
  parent object, it shouldn't be an actor
- If an object doesn't need to send or receive activities, even if it's self
  contained, there's probably no need to make it an actor, because it
  practically doesn't participate in actor-model communication

Examples:

- A ticket/issue/bug is created with respect to some project, repo, software,
  system, the ticket is inherently a part of that parent object, so tickets
  would generally not be actors
- A project or repository are generally self-contained entities, and even if
  some forge has users as top-level namespace and repos are created under
  users, the user managing/owning/sharing a repo is just a matter of access
  control and authority, *it isn't a part of the meaning of the repo itself*,
  and the repo could easily change hands and change maintainers while remaining
  the same repo, same software, same content, same meaning. So, repos and
  projects would generally be actors.
- A group/organization/team is a self-contained object, a set of users along
  with access control and roles and so on, and it needs to be able to receive
  update activities that update the team members list, structure and access and
  so on, even though a team isn't a user and probably doesn't publish
  activities. So, teams would generally be actors.

#### (2) Authorization and roles

#### (3) Comments

Comments are `Note` objects, published using the `Create` activity.
Requirements, suggestions and details:

- The AS2 `context` property must be specified, and must be a single value, and
  refers to the discussion topic, which is a ticket or a merge request or a
  patch or something else.
- The AS2 `inReplyTo` property must be provided, and must be a single value,
  and either specifies the same value as `context` (which means it's a
  top-level comment under the topic), or specifies another comment, to which it
  replies.
- When receiving a comment with context C and inReplyTo some existing comment
  message M, verify that the context of M is C too
- Some objects that are discussion topics, such as tickets, have a `followers`
  collection, which should include all the previous commenters on the topic (if
  you comment on a ticket, you probably want to start following it to be
  notified on new comments, although this isn't required, so that people can
  opt in and opt out of notifications) as well as anyone who sent a Follow
  activity even without commenting, as well as the topic author (e.g. ticket
  author), and this `followers` collection can be used for comment audience.
- Some objects similarly have a `team` collection (a new proposed ForgeFed
  property). The `team` collection would include people who manage the object.
  While `followers` is usually opt-in, `team` is usually opt-out: For example,
  in a project managed by 3 people, the default could be that all 3 of them get
  notified on every new comment on every new ticket, but once a ticket is
  assigned to one of them, the others can opt out of notifications to reduce
  the noise in their personal inboxes. But how `team` works is
  behind-the-scenes for comments: Just consider it to be a collection of team
  members managing the objects and who want to be notified on new comments.
- Some objects that are discussion topics are actors, or are managed by an
  actor (for example tickets may exist under projects, and projects are
  actors), and when you deliver the comment to them, they store and display it
  in the appropriate discussion page
- Normally, the audience of a new comment would include:
  * The discussion topic (if it's an actor) or the actor that manages it (e.g.
    the project to which the ticket belongs, on which you're commenting)
  * The topic's followers collection
  * The topic's team collection
  * Follower collections of objects/actors that manage the topic (e.g. a ticket
    comment may be addressed to the project's followers)
  * Specific individuals/groups you want to mention or bring the discussion to
    their attention
- This setup with a followers collection means that clients don't need to do
  any digging and querying to figure out who the commenters and team members
  are, and it allows people to opt out of notifications if they previously
  commented on some topic but don't want to be in the discussion anymore. This
  makes it very easy for clients to correctly address comments.
- The wording in the ActivityPub spec implies that `followers` is a property of
  actors, and I'm unsure whether it's safe and compatible to use it with
  non-actors. So until this point is discussed, the temporary proposed name for
  the followers collection is `participants`.

`GET /luke/outbox/A0O8l`

```json
{
    "@context": "https://www.w3.org/ns/activitystreams",
    "id": "https://dev.federated.coop/luke/outbox/A0O8l",
    "type": "Create",
    "to": [
        "https://dev.federated.coop/luke/text-adventure",
        "https://dev.federated.coop/luke/text-adventure/followers",
        "https://dev.federated.coop/luke/text-adventure/issues/113/followers",
        "https://dev.federated.coop/luke/text-adventure/issues/113/team"
    ],
    "actor": "https://dev.federated.coop/luke",
    "object": {
        "id": "https://dev.federated.coop/luke/comments/L0dRp",
        "type": "Note",
        "attributedTo": "https://dev.federated.coop/luke",
        "context": "https://dev.federated.coop/luke/text-adventure/issues/113",
        "published": "2019-05-26T11:56:50.024267645Z",
        "to": [
            "https://dev.federated.coop/luke/text-adventure",
            "https://dev.federated.coop/luke/text-adventure/followers",
            "https://dev.federated.coop/luke/text-adventure/issues/113/followers",
            "https://dev.federated.coop/luke/text-adventure/issues/113/team"
        ],
        "content": "That's such a wonderful idea!",
        "inReplyTo": "https://poetry.space/aviva/comments/xN82v"
    }
}
```

TODO:

- `replies` and how the C2S object nesting depth proposal
- Use `nonActors` in the example?
- Content format? HTML? Markdown source? Tags? Referenced ticktes?
- Visibility and privacy?

#### (4) Tickets

While comments are published and hosted by the actors who write them, a
project's tickets are hosted by the project. Actors may still host their
original copy of a ticket. I suppose in general we could allow tickets to be
published first, and then offered in a separate activity, and we could allow
projects to list tickets hosted on other servers. TODO discuss these two
things. In Vervis, at the time of writing, a ticket is offered without a prior
publishing activity, and projects all host their tickets.

A ticket is published using an Offer activity. The author actor may address the
project's followers, but the server may decide not to deliver to them (or
perhaps deliver to them only if the ticket is accepted). In Vervis, project
followers get delivered to. Also in Vervis, a ticket's deps and rdeps can only
be tickets under the same project, but this restriction will hopefully be
relaxed in the future.

The Offer activity looks more-or-less like this:

`GET /aviva/activities/c8lrd0`

```json
{ "@context":
    [ "https://www.w3.org/ns/activitystreams"
    , { "forge":     "https://forgefed.peers.community/ns#"
      , "Ticket":    "forge:Ticket"
      , "isResolved": "forge:isResolved"
      , "dependsOn":
          { "@id":   "forge:dependsOn"
          , "@type": "@id"
          }
      }
    ]
, "id":           "https://https://poetry.space/aviva/activities/c8lrd0"
, "type":         "Offer"
, "to":
    [ "https://poetry.space/aviva/followers"
    , "https://dev.federated.coop/luke/text-adventure"
    , "https://dev.federated.coop/luke/text-adventure/team"
    , "https://dev.federated.coop/luke/text-adventure/followers"
    ]
, "summary":      "<p>Aviva offered a ticket to project text-adventure.</p>"
, "target":       "https://dev.federated.coop/luke/text-adventure"
, "object":
    { "type":         "Ticket"
    , "attributedTo": "https://poetry.space/aviva"
    , "published":    "2019-02-17T11:31:33Z"
    , "summary":      "<p>Game crashes when tasting the coconut cream</p>"
    , "content":      "..."
    , "mediaType":    "text/html"
    , "source":
        { "content":   "..."
        , "mediaType": "text/markdown"
        }
    , "isResolved":   false
    , "dependsOn":
        [ "https://dev.federated.coop/luke/text-adventure/issues/106"
        , "https://dev.community/jerry/text-game-engine/issues/1219"
        ]
    }
}
```

If the ticket is accepted (which may happen automatically or manually, in
Vervis currently always happens automatically), the project's server gives it
an ID and hosts a copy in the project's ticket tracker. The Ticket object may
look like this:

`GET /luke/text-adventure/issues/113`

```json
{ "@context":
    [ "https://www.w3.org/ns/activitystreams"
    , { "forge":     "https://forgefed.peers.community/ns#"
      , "ext":       "https://peers.community/as2-ext#"
      , "Ticket":    "forge:Ticket"
      , "assignedTo":
          { "@id":   "forge:assignedTo"
          , "@type": "@id"
          }
      , "isResolved": "forge:isResolved"
      , "participants":
          { "@id":   "ext:participants"
          , "@type": "@id"
          }
      , "team":
          { "@id":   "ext:team"
          , "@type": "@id"
          }
      , "dependsOn":
          { "@id":   "forge:dependsOn"
          , "@type": "@id"
          }
      , "dependedBy":
          { "@id":   "forge:dependedBy"
          , "@type": "@id"
          }
      , "history":
          { "@id":   "ext:history"
          , "@type": "@id"
          }
      }
    ]
, "id":           "https://dev.federated.coop/luke/text-adventure/issues/113"
, "type":         "Ticket"
, "attributedTo": "https://poetry.space/aviva"
, "published":    "2019-02-17T11:31:33Z"
, "updated":      "2019-06-01T12:30:36Z"
, "context":      "https://dev.federated.coop/luke/text-adventure"
, "name":         "#113"
, "summary":      "<p>Game crashes when tasting the coconut cream</p>"
, "content":      "..."
, "mediaType":    "text/html"
, "source":
    { "content":   "..."
    , "mediaType": "text/markdown"
    }
, "replies":
    [ https://dev.federated.coop/users/luke/posts/vr7mnt9
    , https://dev.community/jerry/outbox/n3y0rk
    ]
, "assignedTo":   "https://dev.community/jerry"
, "isResolved":   false
, "participants": "https://dev.federated.coop/luke/text-adventure/issues/113/participants"
, "team":         "https://dev.federated.coop/luke/text-adventure/issues/113/team"
, "dependsOn":
    [ "https://dev.federated.coop/luke/text-adventure/issues/106"
    , "https://dev.community/jerry/text-game-engine/issues/1219"
    ]
, "dependedBy": "https://dev.federated.coop/luke/text-adventure/issues/87"
, "history":
    [ "https://https://poetry.space/aviva/activities/c8lrd0"
    , "https://dev.federated.coop/luke/text-adventure/outbox/b3r1shv4"
    ]
}
```

The Accept activity can be sent automatically by the Project actor, or manually
by a Person in the project team. It has `object` set to the URI of the Offer,
and `result` set to the URI of the newly created Ticket.

```json
{
    "summary": "<p>fr33's ticket accepted by project ./s/fr33/p/sandbox: This ticket is open</p>",
    "@context": [
        "https://www.w3.org/ns/activitystreams",
        "https://forgefed.peers.community/ns",
        "https://angeley.es/as2-ext"
    ],
    "to": [
        "https://forge.angeley.es/s/fr33",
        "https://forge.angeley.es/s/fr33/p/sandbox/team",
        "https://forge.angeley.es/s/fr33/p/sandbox/followers"
    ],
    "actor": "https://forge.angeley.es/s/fr33/p/sandbox",
    "result": "https://forge.angeley.es/s/fr33/p/sandbox/t/6",
    "object": "https://forge.angeley.es/s/fr33/outbox/wl5Yl",
    "id": "https://forge.angeley.es/s/fr33/p/sandbox/outbox/r07JE",
    "type": "Accept"
}
```

TODO turn replies and history into URIS pointing to separate Collections

TODO replies and depends (ForgeFed #12)

TODO content/source and media types (ForgeFed #11)

#### (5) Patches

#### (6) Merge requests

#### (7) Commits

#### (8) Forks

#### (9) SSH keys

#### (10) Pushes

#### (11) Avatars

Proposal:

- Use Libravatar for user avatars, at least as an alternative to locally-hosted
  avatars
- Support in Libravatar for getting the avatar by a fediverse ID? Even if it
  isn't an OpenID? Or does that already work?
- Have forges be Libravatar servers?
