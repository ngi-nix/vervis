{- This file is part of Vervis.
 -
 - Written in 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
 -
 - ♡ Copying is an act of love. Please copy, reuse and share.
 -
 - The author(s) have dedicated all copyright and related and neighboring
 - rights to this software to the public domain worldwide. This software is
 - distributed without any warranty.
 -
 - You should have received a copy of the CC0 Public Domain Dedication along
 - with this software. If not, see
 - <http://creativecommons.org/publicdomain/zero/1.0/>.
 -}

module Web.ActivityPub
    ( -- * Type-safe manipulation tools
      --
      -- Types and functions that make handling URIs and JSON-LD contexts less
      -- error-prone and safer by recording safety checks in the type and
      -- placing the checks in a single clear place.
      ActivityPub (..)
    , Doc (..)

      -- * Actor
      --
      -- ActivityPub actor document including a public key, with a 'FromJSON'
      -- instance for fetching and a 'ToJSON' instance for publishing.
    , ActorType (..)
    --, Algorithm (..)
    , Owner (..)
    , PublicKey (..)
    , SshKeyAlgorithm (..)
    , SshPublicKey (..)
    , Actor (..)
    , Repo (..)
    , Project (..)
    , CollectionType (..)
    , Collection (..)
    , CollectionPageType (..)
    , CollectionPage (..)
    , Recipient (..)

      -- * Content objects
    , Note (..)
    , TicketDependency (..)
    , TextHtml (..)
    , TextPandocMarkdown (..)
    , PatchType (..)
    , PatchLocal (..)
    , Patch (..)
    , TicketLocal (..)
    , MergeRequest (..)
    , Ticket (..)
    , Author (..)
    , Hash (..)
    , Commit (..)
    , Branch (..)

      -- * Activity
    , Accept (..)
    , CreateObject (..)
    , Create (..)
    , Follow (..)
    , OfferObject (..)
    , Offer (..)
    , Push (..)
    , Reject (..)
    , Resolve (..)
    , Undo (..)
    , Audience (..)
    , SpecificActivity (..)
    , Activity (..)

      -- * Utilities
    , emptyAudience
    , emptyActivity
    , hActivityPubActor
    , provideAP
    , provideAP'
    , APGetError (..)
    , httpGetAP
    , APPostError (..)
    , hActivityPubForwarder
    , hForwardingSignature
    , hForwardedSignature
    , httpPostAP
    , httpPostAPBytes
    , Fetched (..)
    , fetchAP
    , fetchAPID
    , fetchAPID'
    , fetchRecipient
    , keyListedByActor
    , fetchUnknownKey
    , fetchKnownPersonalKey
    , fetchKnownSharedKey

    , Obj (..)
    )
where

import Control.Applicative ((<|>), optional)
import Control.Exception (Exception, displayException, try)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer (Writer)
import Crypto.Hash hiding (Context)
import Data.Aeson
import Data.Aeson.Encoding (pair)
import Data.Aeson.Types (Parser, typeMismatch, listEncoding)
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.Char
import Data.Foldable (for_)
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Semigroup (Endo, First (..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8, decodeUtf8')
import Data.Time.Clock (UTCTime)
import Data.Traversable
import Network.HTTP.Client hiding (Proxy, proxy)
import Network.HTTP.Client.Conduit.ActivityPub (httpAPEither)
import Network.HTTP.Simple (JSONException)
import Network.HTTP.Types.Header (HeaderName, hContentType)
import Text.Email.Parser (EmailAddress)
import Text.HTML.SanitizeXSS
import Yesod.Core.Content (ContentType)
import Yesod.Core.Handler (ProvidedRep, provideRepType)

import Network.HTTP.Client.Signature

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as M
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Signature as S
import qualified Text.Email.Parser as E

import Crypto.PublicVerifKey
import Network.FedURI
import Network.HTTP.Digest

import Data.Aeson.Local

proxy :: a u -> Proxy a
proxy _ = Proxy

as2Context :: Text
as2Context = "https://www.w3.org/ns/activitystreams"

secContext :: Text
secContext = "https://w3id.org/security/v1"

forgeContext :: Text
forgeContext = "https://forgefed.peers.community/ns"

extContext :: Text
extContext = "https://angeley.es/as2-ext"

publicURI :: Text
publicURI = "https://www.w3.org/ns/activitystreams#Public"

class ActivityPub a where
    jsonldContext :: Proxy a -> [Text]
    parseObject   :: UriMode u => Object -> Parser (Authority u, a u)
    toSeries      :: UriMode u => Authority u -> a u -> Series

data Doc a u = Doc
    { docAuthority :: Authority u
    , docValue     :: a u
    }

instance (ActivityPub a, UriMode u) => FromJSON (Doc a u) where
    parseJSON = withObject "Doc" $ \ o -> uncurry Doc <$> parseObject o

instance (ActivityPub a, UriMode u) => ToJSON (Doc a u) where
    toJSON = error "toJSON Doc"
    toEncoding (Doc h v) =
        pairs
            $  context (jsonldContext $ proxy v)
            <> toSeries h v
        where
        context []  = mempty
        context [t] = "@context" .= t
        context ts  = "@context" .= ts

data ActorType =
    ActorTypePerson | ActorTypeRepo | ActorTypeProject | ActorTypeOther Text
    deriving Eq

instance FromJSON ActorType where
    parseJSON = withText "ActorType" $ pure . parse
        where
        parse t
            | t == "Person"     = ActorTypePerson
            | t == "Repository" = ActorTypeRepo
            | t == "Project"    = ActorTypeProject
            | otherwise         = ActorTypeOther t

instance ToJSON ActorType where
    toJSON = error "toJSON ActorType"
    toEncoding at =
        toEncoding $ case at of
            ActorTypePerson  -> "Person"
            ActorTypeRepo    -> "Repository"
            ActorTypeProject -> "Project"
            ActorTypeOther t -> t

data Owner = OwnerInstance | OwnerActor LocalURI

ownerShared :: Owner -> Bool
ownerShared OwnerInstance  = True
ownerShared (OwnerActor _) = False

data PublicKey u = PublicKey
    { publicKeyId       :: LocalRefURI
    , publicKeyExpires  :: Maybe UTCTime
    , publicKeyOwner    :: Owner
    , publicKeyMaterial :: PublicVerifKey
    }

instance ActivityPub PublicKey where
    jsonldContext _ = [secContext, extContext]
    parseObject o = do
        mtyp <- optional $ o .: "@type" <|> o .: "type"
        for_ mtyp $ \ t ->
            unless (t == ("Key" :: Text) || t == "CryptographicKey") $
                fail "PublicKey @type isn't Key or CryptographicKey"
        RefURI authority id_ <- o .: "@id" <|> o .: "id"
        shared <- o .:|? "isShared" .!= False
        fmap (authority,) $
            PublicKey id_
                <$> o .:? "expires"
                <*> (mkOwner shared =<< withAuthorityO authority (o .: "owner"))
                <*> (either fail return . decodePublicVerifKeyPEM =<<
                        o .: "publicKeyPem"
                    )
        where
        mkOwner True  lu
            | lu == topLocalURI = return OwnerInstance
        mkOwner True  _         = fail "Shared key but owner isn't instance URI"
        mkOwner False lu        = return $ OwnerActor lu
    toSeries authority (PublicKey id_ mexpires owner mat)
        =  "@id"          .=  RefURI authority id_
        <> "expires"      .=? mexpires
        <> "owner"        .=  mkOwner authority owner
        <> "publicKeyPem" .=  encodePublicVerifKeyPEM mat
        <> "isShared"     .=  ownerShared owner
        where
        mkOwner a OwnerInstance   = ObjURI a topLocalURI
        mkOwner a (OwnerActor lu) = ObjURI a lu

parsePublicKeySet
    :: UriMode u
    => Value
    -> Parser (Authority u, [Either LocalURI (PublicKey u)])
parsePublicKeySet v =
    case v of
        Array a ->
            case V.toList a of
                [] -> fail "No public keys"
                k : ks -> do
                    (a, e) <- parseKey k
                    es <- traverse (withAuthorityT a . parseKey) ks
                    return (a, e : es)
        _ -> second (: []) <$> parseKey v
    where
    parseKey v@(String _) = second Left . f2l <$> parseJSON v
        where
        f2l (ObjURI a l)  = (a, l)
    parseKey (Object o)   = second Right <$> parseObject o
    parseKey v            = typeMismatch "PublicKeySet Item" v

encodePublicKeySet
    :: UriMode u => Authority u -> [Either LocalURI (PublicKey u)] -> Encoding
encodePublicKeySet authority es =
    case es of
        [e] -> renderKey e
        _   -> listEncoding renderKey es
    where
    renderKey (Left lu)  = toEncoding $ ObjURI authority lu
    renderKey (Right pk) = pairs $ toSeries authority pk

data SshKeyAlgorithm
    = SshKeyAlgorithmRSA
    | SshKeyAlgorithmDSA
    | SshKeyAlgorithmECDSA
    | SshKeyAlgorithmEd25519

instance FromJSON SshKeyAlgorithm where
    parseJSON = withText "SshKeyAlgorithm" parse
        where
        parse t
            | t == "ssh-rsa"     = pure SshKeyAlgorithmRSA
            | t == "ssh-dsa"     = pure SshKeyAlgorithmDSA
            | t == "ssh-ecdsa"   = pure SshKeyAlgorithmECDSA
            | t == "ssh-ed25519" = pure SshKeyAlgorithmEd25519
            | otherwise          =
                fail $ "Unrecognized ssh key algo: " ++ T.unpack t

instance ToJSON SshKeyAlgorithm where
    toJSON = error "toJSON SshKeyAlgorithm"
    toEncoding = toEncoding . render
        where
        render :: SshKeyAlgorithm -> Text
        render SshKeyAlgorithmRSA     = "ssh-rsa"
        render SshKeyAlgorithmDSA     = "ssh-dsa"
        render SshKeyAlgorithmECDSA   = "ssh-ecdsa"
        render SshKeyAlgorithmEd25519 = "ssh-ed25519"

data SshPublicKey u = SshPublicKey
    { sshPublicKeyId        :: LocalURI
    , sshPublicKeyExpires   :: Maybe UTCTime
    , sshPublicKeyOwner     :: LocalURI
    , sshPublicKeyAlgorithm :: SshKeyAlgorithm
    , sshPublicKeyMaterial  :: ByteString
    }

instance ActivityPub SshPublicKey where
    jsonldContext _ = [secContext, forgeContext, extContext]
    parseObject o = do
        mtyp <- optional $ o .: "@type" <|> o .: "type"
        for_ mtyp $ \ t ->
            when (t /= ("SshKey" :: Text)) $
                fail "SshKey @type isn't SshKey"

        mediaType <- o .: "mediaType"
        unless (mediaType == ("application/octet-stream" :: Text)) $
            fail "mediaType isn't octet-stream"

        ObjURI authority luId <- o .: "@id" <|> o .: "id"
        fmap (authority,) $
            SshPublicKey luId
                <$> o .:? "expires"
                <*> withAuthorityO authority (o .: "owner")
                <*> o .: "sshKeyType"
                <*> (decodeBase64 . encodeUtf8 =<< o .: "content")
        where
        decodeBase64 = either fail return . B64.decode
    toSeries authority (SshPublicKey luId mexpires owner algo mat)
        =  "@id"        .=  ObjURI authority luId
        <> "expires"    .=? mexpires
        <> "owner"      .=  ObjURI authority owner
        <> "sshKeyType" .=  algo
        <> "mediaType"  .=  ("application/octet-stream" :: Text)
        <> "content"    .=  decodeUtf8 (B64.encode mat)

data Actor u = Actor
    { actorId         :: LocalURI
    , actorType       :: ActorType
    , actorUsername   :: Maybe Text
    , actorName       :: Maybe Text
    , actorSummary    :: Maybe Text
    , actorInbox      :: LocalURI
    , actorOutbox     :: Maybe LocalURI
    , actorFollowers  :: Maybe LocalURI
    , actorFollowing  :: Maybe LocalURI
    , actorPublicKeys :: [Either LocalURI (PublicKey u)]
    , actorSshKeys    :: [LocalURI]
    }

instance ActivityPub Actor where
    jsonldContext _ = [as2Context, secContext, forgeContext, extContext]
    parseObject o = do
        ObjURI authority id_ <- o .: "id"
        fmap (authority,) $
            Actor id_
                <$> o .: "type"
                <*> o .:? "preferredUsername"
                <*> o .:? "name"
                <*> o .:? "summary"
                <*> withAuthorityO authority (o .: "inbox")
                <*> withAuthorityMaybeO authority (o .:? "outbox")
                <*> withAuthorityMaybeO authority (o .:? "followers")
                <*> withAuthorityMaybeO authority (o .:? "following")
                <*> withAuthorityT authority (parsePublicKeySet =<< o .: "publicKey")
                <*> (traverse (withAuthorityO authority . return) =<< o .:? "sshKey" .!= [])
    toSeries authority
        (Actor id_ typ musername mname msummary inbox outbox followers following pkeys skeys)
            =  "id"                .=     ObjURI authority id_
            <> "type"              .=     typ
            <> "preferredUsername" .=?    musername
            <> "name"              .=?    mname
            <> "summary"           .=?    msummary
            <> "inbox"             .=     ObjURI authority inbox
            <> "outbox"            .=?    (ObjURI authority <$> outbox)
            <> "followers"         .=?    (ObjURI authority <$> followers)
            <> "following"         .=?    (ObjURI authority <$> following)
            <> "publicKey"         `pair` encodePublicKeySet authority pkeys
            <> "sshKey"            .=%    map (ObjURI authority) skeys

data Repo u = Repo
    { repoActor :: Actor u
    , repoTeam  :: LocalURI
    }

instance ActivityPub Repo where
    jsonldContext _ = [as2Context, secContext, forgeContext, extContext]
    parseObject o = do
        (h, a) <- parseObject o
        unless (actorType a == ActorTypeRepo) $
            fail "Actor type isn't Repository"
        fmap (h,) $
            Repo a
                <$> withAuthorityO h (o .:| "team")
    toSeries authority (Repo actor team)
        =  toSeries authority actor
        <> "team" .= ObjURI authority team

data Project u = Project
    { projectActor :: Actor u
    , projectTeam  :: LocalURI
    }

instance ActivityPub Project where
    jsonldContext _ = [as2Context, secContext, forgeContext, extContext]
    parseObject o = do
        (h, a) <- parseObject o
        unless (actorType a == ActorTypeProject) $
            fail "Actor type isn't Project"
        fmap (h,) $
            Project a
                <$> withAuthorityO h (o .:| "team")
    toSeries authority (Project actor team)
        =  toSeries authority actor
        <> "team" .= ObjURI authority team

data CollectionType = CollectionTypeUnordered | CollectionTypeOrdered

instance FromJSON CollectionType where
    parseJSON = withText "CollectionType" parse
        where
        parse "Collection"        = pure CollectionTypeUnordered
        parse "OrderedCollection" = pure CollectionTypeOrdered
        parse t = fail $ "Unknown collection type: " ++ T.unpack t

instance ToJSON CollectionType where
    toJSON = error "toJSON CollectionType"
    toEncoding ct =
        toEncoding $ case ct of
            CollectionTypeUnordered -> "Collection" :: Text
            CollectionTypeOrdered   -> "OrderedCollection"

data Collection a u = Collection
    { collectionId         :: LocalURI
    , collectionType       :: CollectionType
    , collectionTotalItems :: Maybe Int
    , collectionCurrent    :: Maybe LocalURI
    , collectionFirst      :: Maybe LocalPageURI
    , collectionLast       :: Maybe LocalPageURI
    , collectionItems      :: [a]
    }

instance (FromJSON a, ToJSON a) => ActivityPub (Collection a) where
    jsonldContext _ = [as2Context, forgeContext, extContext]
    parseObject o = do
        ObjURI authority id_ <- o .: "id"
        fmap (authority,) $
            Collection id_
                <$> o .: "type"
                <*> o .:? "totalItems"
                <*> withAuthorityMaybeO authority (o .:? "current")
                <*> withAuthorityMaybeP authority (o .:? "first")
                <*> withAuthorityMaybeP authority (o .:? "last")
                <*> optional (o .: "items" <|> o .: "orderedItems") .!= []
    toSeries authority (Collection id_ typ total curr firzt last items)
        =  "id"         .=  ObjURI authority id_
        <> "type"       .=  typ
        <> "totalItems" .=? total
        <> "current"    .=? (ObjURI authority <$> curr)
        <> "first"      .=? (PageURI authority <$> firzt)
        <> "last"       .=? (PageURI authority <$> last)
        <> itemsProp    .=% items
        where
        itemsProp =
            case typ of
                CollectionTypeUnordered -> "items"
                CollectionTypeOrdered -> "orderedItems"

data CollectionPageType
    = CollectionPageTypeUnordered
    | CollectionPageTypeOrdered

instance FromJSON CollectionPageType where
    parseJSON = withText "CollectionPageType" parse
        where
        parse "CollectionPage"        = pure CollectionPageTypeUnordered
        parse "OrderedCollectionPage" = pure CollectionPageTypeOrdered
        parse t = fail $ "Unknown collection page type: " ++ T.unpack t

instance ToJSON CollectionPageType where
    toJSON = error "toJSON CollectionPageType"
    toEncoding ct =
        toEncoding $ case ct of
            CollectionPageTypeUnordered -> "CollectionPage" :: Text
            CollectionPageTypeOrdered   -> "OrderedCollectionPage"

data CollectionPage a u = CollectionPage
    { collectionPageId         :: LocalPageURI
    , collectionPageType       :: CollectionPageType
    , collectionPageTotalItems :: Maybe Int
    , collectionPageCurrent    :: Maybe LocalPageURI
    , collectionPageFirst      :: Maybe LocalPageURI
    , collectionPageLast       :: Maybe LocalPageURI
    , collectionPagePartOf     :: LocalURI
    , collectionPagePrev       :: Maybe LocalPageURI
    , collectionPageNext       :: Maybe LocalPageURI
    , collectionPageStartIndex :: Maybe Int
    , collectionPageItems      :: [a]
    }

instance (FromJSON a, ToJSON a) => ActivityPub (CollectionPage a) where
    jsonldContext _ = [as2Context, forgeContext, extContext]
    parseObject o = do
        PageURI authority id_ <- o .: "id"
        fmap (authority,) $
            CollectionPage id_
                <$> o .: "type"
                <*> o .:? "totalItems"
                <*> withAuthorityMaybeP authority (o .:? "current")
                <*> withAuthorityMaybeP authority (o .:? "first")
                <*> withAuthorityMaybeP authority (o .:? "last")
                <*> withAuthorityO authority (o .: "partOf")
                <*> withAuthorityMaybeP authority (o .:? "prev")
                <*> withAuthorityMaybeP authority (o .:? "next")
                <*> o .:? "startIndex"
                <*> optional (o .: "items" <|> o .: "orderedItems") .!= []
    toSeries authority (CollectionPage id_ typ total curr firzt last partOf prev next ind items)
        =  "id"         .=  PageURI authority id_
        <> "type"       .=  typ
        <> "totalItems" .=? total
        <> "current"    .=? (PageURI authority <$> curr)
        <> "first"      .=? (PageURI authority <$> firzt)
        <> "last"       .=? (PageURI authority <$> last)
        <> "partOf"     .=  (ObjURI authority partOf)
        <> "prev"       .=? (PageURI authority <$> prev)
        <> "next"       .=? (PageURI authority <$> next)
        <> "startIndex" .=? ind
        <> itemsProp    .=% items
        where
        itemsProp =
            case typ of
                CollectionPageTypeUnordered -> "items"
                CollectionPageTypeOrdered -> "orderedItems"

data Recipient u = RecipientActor (Actor u) | RecipientCollection (Collection (ObjURI u) u)

instance ActivityPub Recipient where
    jsonldContext _ = [as2Context, secContext, forgeContext, extContext]
    parseObject o =
        second RecipientActor <$> parseObject o <|>
        second RecipientCollection <$> parseObject o
    toSeries h (RecipientActor a)      = toSeries h a
    toSeries h (RecipientCollection c) = toSeries h c

data Audience u = Audience
    { audienceTo        :: [ObjURI u]
    , audienceBto       :: [ObjURI u]
    , audienceCc        :: [ObjURI u]
    , audienceBcc       :: [ObjURI u]
    , audienceGeneral   :: [ObjURI u]
    , audienceNonActors :: [ObjURI u]
    }

newtype AdaptAudience u = AdaptAudience
    { unAdapt :: ObjURI u
    }

instance UriMode u => FromJSON (AdaptAudience u) where
    parseJSON = fmap AdaptAudience . parseJSON . adapt
        where
        adapt v =
            case v of
                String t
                    | t == "as:Public" -> String "Public"
                    | t == publicURI   -> String "Public"
                _ -> v

parseAudience :: UriMode u => Object -> Parser (Audience u)
parseAudience o =
    Audience
        <$> o .:& "to"
        <*> o .:& "bto"
        <*> o .:& "cc"
        <*> o .:& "bcc"
        <*> o .:& "audience"
        <*> o .:|& "nonActors"
    where
    obj .:& key = do
        l <- obj .:? key .!= []
        return $ map unAdapt l
    obj .:|& key = do
        l <- obj .:|? key .!= []
        return $ map unAdapt l

encodeAudience :: UriMode u => Audience u -> Series
encodeAudience (Audience to bto cc bcc aud nons)
    =  "to"        .=% to
    <> "bto"       .=% bto
    <> "cc"        .=% cc
    <> "bcc"       .=% bcc
    <> "audience"  .=% aud
    <> "nonActors" .=% nons

data Note u = Note
    { noteId        :: Maybe LocalURI
    , noteAttrib    :: LocalURI
    , noteAudience  :: Audience u
    , noteReplyTo   :: Maybe (ObjURI u)
    , noteContext   :: Maybe (ObjURI u)
    , notePublished :: Maybe UTCTime
    , noteSource    :: Text
    , noteContent   :: Text
    }

withAuthorityT a m = do
    (a', v) <- m
    if a == a'
        then return v
        else fail "URI authority mismatch"

withAuthorityO a m = do
    ObjURI a' v <- m
    if a == a'
        then return v
        else fail "URI authority mismatch"

withAuthorityS a m = do
    SubURI a' v <- m
    if a == a'
        then return v
        else fail "URI authority mismatch"

withAuthorityP a m = do
    PageURI a' v <- m
    if a == a'
        then return v
        else fail "URI authority mismatch"

withAuthorityMaybeT a m = do
    mu <- m
    for mu $ \ (a', v) ->
        if a == a'
            then return v
            else fail "URI authority mismatch"

withAuthorityMaybeO a m = do
    mu <- m
    for mu $ \ (ObjURI a' v) ->
        if a == a'
            then return v
            else fail "URI authority mismatch"

withAuthorityMaybeS a m = do
    mu <- m
    for mu $ \ (SubURI a' v) ->
        if a == a'
            then return v
            else fail "URI authority mismatch"

withAuthorityMaybeP a m = do
    mu <- m
    for mu $ \ (PageURI a' v) ->
        if a == a'
            then return v
            else fail "URI authority mismatch"

instance ActivityPub Note where
    jsonldContext _ = [as2Context, extContext]
    parseObject o = do
        typ <- o .: "type"
        unless (typ == ("Note" :: Text)) $
            fail "type isn't Note"

        mediaType <- o .: "mediaType"
        unless (mediaType == ("text/html" :: Text)) $
            fail "mediaType isn't HTML"

        source <- o .: "source"
        sourceType <- source .: "mediaType"
        unless (sourceType == ("text/markdown; variant=Pandoc" :: Text)) $
            fail "source mediaType isn't Pandoc Markdown"

        ObjURI a attrib <- o .: "attributedTo"
        fmap (a,) $
            Note
                <$> withAuthorityMaybeO a (o .:? "id")
                <*> pure attrib
                <*> parseAudience o
                <*> o .:? "inReplyTo"
                <*> o .:? "context"
                <*> o .:? "published"
                <*> source .: "content"
                <*> (sanitizeBalance <$> o .: "content")
    toSeries authority (Note mid attrib aud mreply mcontext mpublished src content)
        =  "type"         .=  ("Note" :: Text)
        <> "id"           .=? (ObjURI authority <$> mid)
        <> "attributedTo" .=  ObjURI authority attrib
        <> encodeAudience aud
        <> "inReplyTo"    .=? mreply
        <> "context"      .=? mcontext
        <> "published"    .=? mpublished
        <> "source"       .=  object
                [ "content"   .= src
                , "mediaType" .= ("text/markdown; variant=Pandoc" :: Text)
                ]
        <> "content"      .=  content
        <> "mediaType"    .=  ("text/html" :: Text)

data RelationshipProperty = RelDependsOn deriving Eq

instance FromJSON RelationshipProperty where
    parseJSON = withText "RelationshipProperty" parse
        where
        parse t
            | t == "dependsOn" = pure RelDependsOn
            | otherwise = fail $ "Unrecognized relationship: " ++ T.unpack t

instance ToJSON RelationshipProperty where
    toJSON = error "toJSON RelationshipProperty"
    toEncoding at =
        toEncoding $ case at of
            RelDependsOn -> "dependsOn" :: Text

data Relationship u = Relationship
    { relationshipId           :: Maybe (ObjURI u)
    , relationshipExtraTypes   :: [Text]
    , relationshipSubject      :: ObjURI u
    , relationshipProperty     :: Either RelationshipProperty Text
    , relationshipObject       :: ObjURI u
    , relationshipAttributedTo :: LocalURI
    , relationshipPublished    :: Maybe UTCTime
    , relationshipUpdated      :: Maybe UTCTime
    }

instance ActivityPub Relationship where
    jsonldContext _ = [as2Context, forgeContext]
    parseObject o = do
        typs <- o .: "type"
        unless (("Relationship" :: Text) `elem` typs) $
            fail "type isn't Relationship"

        ObjURI a attributedTo <- o .: "attributedTo"

        fmap (a,) $
            Relationship
                <$> o .:? "id"
                <*> pure (delete "Relationship" typs)
                <*> o .: "subject"
                <*> o .:+ "relationship"
                <*> o .: "object"
                <*> pure attributedTo
                <*> o .:? "published"
                <*> o .:? "updated"

    toSeries authority
        (Relationship id_ typs subject property object attributedTo published
                      updated)
            =  "id"           .=? id_
            <> "type"         .=  ("Relationship" : typs)
            <> "subject"      .=  subject
            <> "relationship" .=+ property
            <> "object"       .=  object
            <> "attributedTo" .=  ObjURI authority attributedTo
            <> "published"    .=? published
            <> "updated"      .=? updated

data TicketDependency u = TicketDependency
    { ticketDepId           :: Maybe (ObjURI u)
    , ticketDepParent       :: ObjURI u
    , ticketDepChild        :: ObjURI u
    , ticketDepAttributedTo :: LocalURI
    , ticketDepPublished    :: Maybe UTCTime
    , ticketDepUpdated      :: Maybe UTCTime
    }

instance ActivityPub TicketDependency where
    jsonldContext _ = [as2Context, forgeContext]
    parseObject o = do
        (a, rel) <- parseObject o
        unless ("TicketDependency" `elem` relationshipExtraTypes rel) $
            fail "type isn't TicketDependency"

        unless (relationshipProperty rel == Left RelDependsOn) $
            fail "relationship isn't dependsOn"

        return (a, rel2td rel)
        where
        rel2td rel = TicketDependency
            { ticketDepId           = relationshipId rel
            , ticketDepParent       = relationshipSubject rel
            , ticketDepChild        = relationshipObject rel
            , ticketDepAttributedTo = relationshipAttributedTo rel
            , ticketDepPublished    = relationshipPublished rel
            , ticketDepUpdated      = relationshipUpdated rel
            }

    toSeries a = toSeries a . td2rel
        where
        td2rel td = Relationship
            { relationshipId           = ticketDepId td
            , relationshipExtraTypes   = ["TicketDependency"]
            , relationshipSubject      = ticketDepParent td
            , relationshipProperty     = Left RelDependsOn
            , relationshipObject       = ticketDepChild td
            , relationshipAttributedTo = ticketDepAttributedTo td
            , relationshipPublished    = ticketDepPublished td
            , relationshipUpdated      = ticketDepUpdated td
            }

newtype TextHtml = TextHtml
    { unTextHtml :: Text
    }
    deriving (FromJSON, ToJSON)

newtype TextPandocMarkdown = TextPandocMarkdown
    { unTextPandocMarkdown :: Text
    }
    deriving (FromJSON, ToJSON)

data PatchType = PatchTypeDarcs

instance FromJSON PatchType where
    parseJSON = withText "PatchType" parse
        where
        parse "application/x-darcs-patch" = pure PatchTypeDarcs
        parse t = fail $ "Unknown patch mediaType: " ++ T.unpack t

instance ToJSON PatchType where
    toJSON = error "toJSON PatchType"
    toEncoding = toEncoding . render
        where
        render PatchTypeDarcs = "application/x-darcs-patch" :: Text

data PatchLocal = PatchLocal
    { patchId              :: LocalURI
    , patchContext         :: LocalURI
    , patchPrevVersions    :: [LocalURI]
    , patchCurrentVersion  :: Maybe LocalURI
    }

parsePatchLocal
    :: UriMode u => Object -> Parser (Maybe (Authority u, PatchLocal))
parsePatchLocal o = do
    mid <- o .:? "id"
    case mid of
        Nothing -> do
            verifyNothing "context"
            verifyNothing "previousVersions"
            verifyNothing "currentVersion"
            return Nothing
        Just (ObjURI a id_) ->
            fmap (Just . (a,)) $
                PatchLocal
                    <$> pure id_
                    <*> withAuthorityO a (o .: "context")
                    <*> (traverse (withAuthorityO a . return) =<< o .:? "previousVersions" .!= [])
                    <*> withAuthorityMaybeO a (o .:? "currentVersion")
    where
    verifyNothing t =
        if t `M.member` o
            then fail $ T.unpack t ++ " field found, expected none"
            else return ()

encodePatchLocal :: UriMode u => Authority u -> PatchLocal -> Series
encodePatchLocal a (PatchLocal id_ context versions mcurrent)
    =  "id"               .= ObjURI a id_
    <> "context"          .= ObjURI a context
    <> "previousVersions" .= map (ObjURI a) versions
    <> "currentVersion"   .=? (ObjURI a <$> mcurrent)

data Patch u = Patch
    { patchLocal        :: Maybe (Authority u, PatchLocal)
    , patchAttributedTo :: LocalURI
    , patchPublished    :: Maybe UTCTime
    , patchType         :: PatchType
    , patchContent      :: Text
    }

instance ActivityPub Patch where
    jsonldContext _ = [as2Context, forgeContext]

    parseObject o = do
        typ <- o .: "type"
        unless (typ == ("Patch" :: Text)) $
            fail "type isn't Patch"

        ObjURI a attrib <- o .: "attributedTo"

        fmap (a,) $
            Patch
                <$> parsePatchLocal o
                <*> pure attrib
                <*> o .:? "published"
                <*> o .: "mediaType"
                <*> o .: "content"

    toSeries a (Patch local attrib published typ content)
        =  maybe mempty (uncurry encodePatchLocal) local
        <> "type"             .= ("Patch" :: Text)
        <> "attributedTo"     .= ObjURI a attrib
        <> "published"        .=? published
        <> "mediaType"        .= typ
        <> "content"          .= content

data TicketLocal = TicketLocal
    { ticketId           :: LocalURI
    , ticketReplies      :: LocalURI
    , ticketParticipants :: LocalURI
    , ticketTeam         :: Maybe LocalURI
    , ticketEvents       :: LocalURI
    , ticketDeps         :: LocalURI
    , ticketReverseDeps  :: LocalURI
    }

parseTicketLocal :: UriMode u => Object -> Parser (Maybe (Authority u, TicketLocal))
parseTicketLocal o = do
    mid <- o .:? "id"
    case mid of
        Nothing -> do
            verifyNothing "replies"
            verifyNothing "participants"
            verifyNothing "followers"
            verifyNothing "team"
            verifyNothing "history"
            verifyNothing "dependencies"
            verifyNothing "dependants"
            return Nothing
        Just (ObjURI a id_) ->
            fmap (Just . (a,)) $
                TicketLocal
                    <$> pure id_
                    <*> withAuthorityO a (o .: "replies")
                    <*> withAuthorityO a (o .: "participants" <|> o .: "followers")
                    <*> withAuthorityMaybeO a (o .:? "team")
                    <*> withAuthorityO a (o .: "history")
                    <*> withAuthorityO a (o .: "dependencies")
                    <*> withAuthorityO a (o .: "dependants")
    where
    verifyNothing t =
        if t `M.member` o
            then fail $ T.unpack t ++ " field found, expected none"
            else return ()

encodeTicketLocal :: UriMode u => Authority u -> TicketLocal -> Series
encodeTicketLocal
    a (TicketLocal id_ replies followers team events deps rdeps)
        =  "id"           .= ObjURI a id_
        <> "replies"      .= ObjURI a replies
        <> "followers"    .= ObjURI a followers
        <> "team"         .=? (ObjURI a <$> team)
        <> "history"      .= ObjURI a events
        <> "dependencies" .= ObjURI a deps
        <> "dependants"   .= ObjURI a rdeps

data MergeRequest u = MergeRequest
    { mrOrigin :: Maybe (ObjURI u)
    , mrTarget :: LocalURI
    , mrPatch  :: Either (ObjURI u) (Authority u, Patch u)
    }

instance ActivityPub MergeRequest where
    jsonldContext _ = [as2Context, forgeContext]

    parseObject o = do
        typ <- o .: "type"
        unless (typ == ("Offer" :: Text)) $
            fail "type isn't Offer"

        ObjURI a target <- o .: "target"

        fmap (a,) $
            MergeRequest
                <$> o .:? "origin"
                <*> pure target
                <*> (second fromDoc . toEither <$> o .: "object")
        where
        fromDoc (Doc h v) = (h, v)

    toSeries h (MergeRequest morigin target patch)
        =  "type"   .=  ("Offer" :: Text)
        <> "origin" .=? morigin
        <> "target" .=  ObjURI h target
        <> "object" .=  fromEither (second (uncurry Doc) patch)

data Ticket u = Ticket
    { ticketLocal        :: Maybe (Authority u, TicketLocal)
    , ticketAttributedTo :: LocalURI
    , ticketPublished    :: Maybe UTCTime
    , ticketUpdated      :: Maybe UTCTime
    , ticketContext      :: Maybe (ObjURI u)
    -- , ticketName         :: Maybe Text
    , ticketSummary      :: TextHtml
    , ticketContent      :: TextHtml
    , ticketSource       :: TextPandocMarkdown
    , ticketAssignedTo   :: Maybe (ObjURI u)
    , ticketResolved     :: Maybe (Maybe (ObjURI u), Maybe UTCTime)
    , ticketAttachment   :: Maybe (Authority u, MergeRequest u)
    }

instance ActivityPub Ticket where
    jsonldContext _ = [as2Context, forgeContext, extContext]
    parseObject o = do
        typ <- o .: "type"
        unless (typ == ("Ticket" :: Text)) $
            fail "type isn't Ticket"

        mediaType <- o .: "mediaType"
        unless (mediaType == ("text/html" :: Text)) $
            fail "mediaType isn't HTML"

        source <- o .: "source"
        sourceType <- source .: "mediaType"
        unless (sourceType == ("text/markdown; variant=Pandoc" :: Text)) $
            fail "source mediaType isn't Pandoc Markdown"

        ObjURI a attributedTo <- o .: "attributedTo"

        mresolved <- do
            is <- o .:? "isResolved" .!= False
            if is
                then do
                    at <- o .:? "resolved"
                    by <- o .:? "resolvedBy"
                    return $ Just (by, at)
                else do
                    verifyNothing "resolved"
                    verifyNothing "resolvedBy"
                    return Nothing

        fmap (a,) $
            Ticket
                <$> parseTicketLocal o
                <*> pure attributedTo
                <*> o .:? "published"
                <*> o .:? "updated"
                <*> o .:? "context"
                -- <*> o .:? "name"
                <*> (TextHtml . sanitizeBalance <$> o .: "summary")
                <*> (TextHtml . sanitizeBalance <$> o .: "content")
                <*> source .: "content"
                <*> o .:? "assignedTo"
                <*> pure mresolved
                <*> (traverse parseObject =<< o .:? "attachment")
        where
        verifyNothing t =
            if t `M.member` o
                then fail $ T.unpack t ++ " field found, expected none"
                else return ()

    toSeries authority
        (Ticket local attributedTo published updated context {-name-}
                summary content source assignedTo mresolved mmr)

            =   maybe mempty (uncurry encodeTicketLocal) local
            <> "type"         .=  ("Ticket" :: Text)
            <> "attributedTo" .=  ObjURI authority attributedTo
            <> "published"    .=? published
            <> "updated"      .=? updated
            <> "context"      .=? context
            -- <> "name"         .=? name
            <> "summary"      .=  summary
            <> "content"      .=  content
            <> "mediaType"    .=  ("text/html" :: Text)
            <> "source"       .=  object
                    [ "content"   .= source
                    , "mediaType" .= ("text/markdown; variant=Pandoc" :: Text)
                    ]
            <> "assignedTo"   .=? assignedTo
            <> maybe
                ("isResolved" .= False)
                (\ (mby, mat)
                    -> "isResolved" .= True
                    <> "resolvedBy" .=? mby
                    <> "resolved"   .=? mat
                )
                mresolved
            <> maybe
                mempty
                (\ (h, mr) -> "attachment" `pair` pairs (toSeries h mr))
                mmr

data Author = Author
    { authorName  :: Text
    , authorEmail :: EmailAddress
    }

instance FromJSON Author where
    parseJSON = withObject "Author" $ \ o ->
        Author
            <$> o .: "name"
            <*> (parseMailto =<< o .: "mbox")
        where
        parseMailto =
            either fail return .
            A.parseOnly (A.string "mailto:" *> E.addrSpec <* A.endOfInput) .
            encodeUtf8

instance ToJSON Author where
    toJSON = error "toJSON Author"
    toEncoding (Author name email) =
        pairs
            $  "name" .= name
            <> "mbox" .= ("mailto:" <> decodeUtf8 (E.toByteString email))

newtype Hash = Hash ByteString

instance FromJSON Hash where
    parseJSON = withText "Hash" $ \ t ->
        let b = encodeUtf8 t
        in  if not (BC.null b) && BC.all isHexDigit b
                then return $ Hash b
                else fail "Hash should be a non-empty hex string"

instance ToJSON Hash where
    toJSON (Hash b) = toJSON $ decodeUtf8 b
    toEncoding (Hash b) = toEncoding $ decodeUtf8 b

data Commit u = Commit
    { commitId          :: LocalURI
    , commitRepository  :: LocalURI
    , commitAuthor      :: Either Author (ObjURI u)
    , commitCommitter   :: Maybe (Either Author (ObjURI u))
    , commitTitle       :: Text
    , commitHash        :: Hash
    , commitDescription :: Maybe Text
    , commitWritten     :: UTCTime
    , commitCommitted   :: Maybe UTCTime
    }

instance ActivityPub Commit where
    jsonldContext _ = [as2Context, forgeContext, extContext]
    parseObject o = do
        typ <- o .: "type"
        unless (typ == ("Commit" :: Text)) $
            fail "type isn't Commit"

        mdesc <- o .:? "description"
        mdescContent <- for mdesc $ \ desc -> do
            descType <- desc .: "mediaType"
            unless (descType == ("text/plain" :: Text)) $
                fail "description mediaType isn't \"text/plain\""
            desc .: "content"

        ObjURI a id_ <- o .: "id"
        fmap (a,) $
            Commit id_
                <$> withAuthorityO a (o .: "context")
                <*> o .:+ "attributedTo"
                <*> o .:+? "committedBy"
                <*> o .: "name"
                <*> o .: "hash"
                <*> pure mdescContent
                <*> o .: "created"
                <*> o .:? "committed"

    toSeries authority
        (Commit id_ repo author committer title hash mdesc written mcommitted)
            =  "id"           .=   ObjURI authority id_
            <> "type"         .=   ("Commit" :: Text)
            <> "context"      .=   ObjURI authority repo
            <> "attributedTo" .=+  author
            <> "committedBy"  .=+? committer
            <> "name"         .=   title
            <> "hash"         .=   hash
            <> maybe
                mempty
                (\ desc -> "description" .= object
                    [ "content"   .= desc
                    , "mediaType" .= ("text/plain" :: Text)
                    ]
                )
                mdesc
            <> "created"      .=   written
            <> "committed"    .=?  mcommitted

data Branch u = Branch
    { branchName :: Text
    , branchRef  :: Text
    , branchRepo :: LocalURI
    }

instance ActivityPub Branch where
    jsonldContext _ = [as2Context, forgeContext]
    parseObject o = do
        typ <- o .: "type"
        unless (typ == ("Branch" :: Text)) $
            fail "type isn't Branch"

        ObjURI a repo <- o .: "context"
        fmap (a,) $
            Branch
                <$> o .: "name"
                <*> o .: "ref"
                <*> pure repo

    toSeries authority (Branch name ref repo)
        =  "type"    .= ("Branch" :: Text)
        <> "name"    .= name
        <> "ref"     .= ref
        <> "context" .= ObjURI authority repo

data Accept u = Accept
    { acceptObject :: ObjURI u
    , acceptResult :: Maybe LocalURI
    }

parseAccept :: UriMode u => Authority u -> Object -> Parser (Accept u)
parseAccept a o =
    Accept
        <$> o .: "object"
        <*> withAuthorityMaybeO a (o .:? "result")

encodeAccept :: UriMode u => Authority u -> Accept u -> Series
encodeAccept authority (Accept obj mresult)
    =  "object" .=  obj
    <> "result" .=? (ObjURI authority <$> mresult)

data CreateObject u = CreateNote (Note u) | CreateTicket (Ticket u)

instance ActivityPub CreateObject where
    jsonldContext = error "jsonldContext CreateObject"
    parseObject o
        =   second CreateNote <$> parseObject o
        <|> second CreateTicket <$> parseObject o
    toSeries au (CreateNote o)   = toSeries au o
    toSeries au (CreateTicket o) = toSeries au o

data Create u = Create
    { createObject :: CreateObject u
    , createTarget :: Maybe (ObjURI u)
    }

parseCreate :: UriMode u => Object -> Authority u -> LocalURI -> Parser (Create u)
parseCreate o a luActor = do
    obj <- withAuthorityT a $ parseObject =<< o .: "object"
    unless (luActor == attrib obj) $ fail "Create actor != object attrib"
    Create obj <$> o .:? "target"
    where
    attrib (CreateNote note)     = noteAttrib note
    attrib (CreateTicket ticket) = ticketAttributedTo ticket

encodeCreate :: UriMode u => Authority u -> LocalURI -> Create u -> Series
encodeCreate authority actor (Create obj target)
    =  "object" `pair` pairs (toSeries authority obj)
    <> "target" .=?    target

data Follow u = Follow
    { followObject  :: ObjURI u
    , followContext :: Maybe (ObjURI u)
    , followHide    :: Bool
    }

parseFollow :: UriMode u => Object -> Parser (Follow u)
parseFollow o =
    Follow
        <$> o .:  "object"
        <*> o .:? "context"
        <*> o .:? "hide" .!= False

encodeFollow :: UriMode u => Follow u -> Series
encodeFollow (Follow obj mcontext hide)
    =  "object"  .=  obj
    <> "context" .=? mcontext
    <> "hide"    .=  hide

data OfferObject u = OfferTicket (Ticket u) | OfferDep (TicketDependency u)

instance ActivityPub OfferObject where
    jsonldContext = error "jsonldContext OfferObject"
    parseObject o
        =   second OfferTicket <$> parseObject o
        <|> second OfferDep    <$> parseObject o
    toSeries h (OfferTicket t) = toSeries h t
    toSeries h (OfferDep d)    = toSeries h d

data Offer u = Offer
    { offerObject :: OfferObject u
    , offerTarget :: ObjURI u
    }

parseOffer :: UriMode u => Object -> Authority u -> LocalURI -> Parser (Offer u)
parseOffer o a luActor = do
    obj <- withAuthorityT a $ parseObject =<< o .: "object"
    target@(ObjURI hTarget luTarget) <- o .: "target"
    case obj of
        OfferTicket ticket -> do
            unless (luActor == ticketAttributedTo ticket) $
                fail "Offer actor != Ticket attrib"
            for_ (ticketContext ticket) $ \ (ObjURI hContext luContext) -> do
                unless (hTarget == hContext) $
                    fail "Offer target host != Ticket context host"
                unless (luTarget == luContext) $
                    fail "Offer target != Ticket context"
        OfferDep dep -> do
            unless (luActor == ticketDepAttributedTo dep) $
                fail "Offer actor != TicketDependency attrib"
    return $ Offer obj target

encodeOffer :: UriMode u => Authority u -> LocalURI -> Offer u -> Series
encodeOffer authority actor (Offer obj target)
    =  "object" `pair` pairs (toSeries authority obj)
    <> "target" .= target

data Push u = Push
    { pushCommitsLast  :: NonEmpty (Commit u)
    , pushCommitsFirst :: Maybe (NonEmpty (Commit u))
    , pushCommitsTotal :: Int
    , pushTarget       :: LocalURI
    , pushContext      :: LocalURI
    , pushHashBefore   :: Maybe Text
    , pushHashAfter    :: Maybe Text
    }

parsePush :: UriMode u => Authority u -> Object -> Parser (Push u)
parsePush a o = do
    c <- o .: "object"
    Push
        <$> (traverse (withAuthorityT a . parseObject) =<< c .: "items" <|> c .: "orderedItems")
        <*> (traverse (traverse $ withAuthorityT a . parseObject) =<< c .:? "earlyItems")
        <*> c .: "totalItems"
        <*> withAuthorityO a (o .: "target")
        <*> withAuthorityO a (o .: "context")
        <*> o .:? "hashBefore"
        <*> o .:? "hashAfter"

encodePush :: UriMode u => Authority u -> Push u -> Series
encodePush a (Push lateCommits earlyCommits total target context before after)
    =  "object"     `pair` pairs
            (  "type"       .= ("OrderedCollection" :: Text)
            <> pair "orderedItems" (objectList lateCommits)
            <> maybe mempty (pair "earlyItems" . objectList) earlyCommits
            <> "totalItems" .= total
            )
    <> "target"     .= ObjURI a target
    <> "context"    .= ObjURI a context
    <> "hashBefore" .=? before
    <> "hashAfter"  .=? after
    where
    objectList items = listEncoding (pairs . toSeries a) (NE.toList items)

data Reject u = Reject
    { rejectObject :: ObjURI u
    }

parseReject :: UriMode u => Object -> Parser (Reject u)
parseReject o = Reject <$> o .: "object"

encodeReject :: UriMode u => Reject u -> Series
encodeReject (Reject obj) = "object" .= obj

data Resolve u = Resolve
    { resolveObject :: ObjURI u
    }

parseResolve :: UriMode u => Object -> Parser (Resolve u)
parseResolve o = Resolve <$> o .: "object"

encodeResolve :: UriMode u => Resolve u -> Series
encodeResolve (Resolve obj) = "object" .= obj

data Undo u = Undo
    { undoObject :: ObjURI u
    }

parseUndo :: UriMode u => Authority u -> Object -> Parser (Undo u)
parseUndo a o = Undo <$> o .: "object"

encodeUndo :: UriMode u => Authority u -> Undo u -> Series
encodeUndo a (Undo obj) = "object" .= obj

data SpecificActivity u
    = AcceptActivity (Accept u)
    | CreateActivity (Create u)
    | FollowActivity (Follow u)
    | OfferActivity  (Offer u)
    | PushActivity   (Push u)
    | RejectActivity (Reject u)
    | ResolveActivity (Resolve u)
    | UndoActivity   (Undo u)

data Activity u = Activity
    { activityId       :: Maybe LocalURI
    , activityActor    :: LocalURI
    , activitySummary  :: Maybe TextHtml
    , activityAudience :: Audience u
    , activitySpecific :: SpecificActivity u
    }

instance ActivityPub Activity where
    jsonldContext _ = [as2Context, forgeContext, extContext]
    parseObject o = do
        ObjURI a actor <- o .: "actor"
        fmap (a,) $
            Activity
                <$> withAuthorityMaybeO a (o .:? "id")
                <*> pure actor
                <*> (fmap (TextHtml . sanitizeBalance) <$> o .:? "summary")
                <*> parseAudience o
                <*> do
                    typ <- o .: "type"
                    case typ of
                        "Accept"  -> AcceptActivity  <$> parseAccept a o
                        "Create"  -> CreateActivity  <$> parseCreate o a actor
                        "Follow"  -> FollowActivity  <$> parseFollow o
                        "Offer"   -> OfferActivity   <$> parseOffer o a actor
                        "Push"    -> PushActivity    <$> parsePush a o
                        "Reject"  -> RejectActivity  <$> parseReject o
                        "Resolve" -> ResolveActivity <$> parseResolve o
                        "Undo"    -> UndoActivity    <$> parseUndo a o
                        _ ->
                            fail $
                                "Unrecognized activity type: " ++ T.unpack typ
    toSeries authority (Activity id_ actor summary audience specific)
        =  "type"    .=  activityType specific
        <> "id"      .=? (ObjURI authority <$> id_)
        <> "actor"   .=  ObjURI authority actor
        <> "summary" .=? summary
        <> encodeAudience audience
        <> encodeSpecific authority actor specific
        where
        activityType :: SpecificActivity u -> Text
        activityType (AcceptActivity _)  = "Accept"
        activityType (CreateActivity _)  = "Create"
        activityType (FollowActivity _)  = "Follow"
        activityType (OfferActivity _)   = "Offer"
        activityType (PushActivity _)    = "Push"
        activityType (RejectActivity _)  = "Reject"
        activityType (ResolveActivity _) = "Resolve"
        activityType (UndoActivity _)    = "Undo"
        encodeSpecific h _ (AcceptActivity a)  = encodeAccept h a
        encodeSpecific h u (CreateActivity a)  = encodeCreate h u a
        encodeSpecific _ _ (FollowActivity a)  = encodeFollow a
        encodeSpecific h u (OfferActivity a)   = encodeOffer h u a
        encodeSpecific h _ (PushActivity a)    = encodePush h a
        encodeSpecific _ _ (RejectActivity a)  = encodeReject a
        encodeSpecific _ _ (ResolveActivity a) = encodeResolve a
        encodeSpecific h _ (UndoActivity a)    = encodeUndo h a

emptyAudience :: Audience u
emptyAudience = Audience [] [] [] [] [] []

emptyActivity :: Activity u
emptyActivity = Activity
    { activityId       = Nothing
    , activityActor    = topLocalURI
    , activitySummary  = Nothing
    , activityAudience = emptyAudience
    , activitySpecific =
        RejectActivity $ Reject $ ObjURI (Authority "" Nothing) topLocalURI
    }

typeActivityStreams2 :: ContentType
typeActivityStreams2 = "application/activity+json"

typeActivityStreams2LD :: ContentType
typeActivityStreams2LD =
    "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\""

hActivityPubActor :: HeaderName
hActivityPubActor = "ActivityPub-Actor"

provideAP :: (Monad m, ToJSON a) => m a -> Writer (Endo [ProvidedRep m]) ()
provideAP mk =
    -- let enc = toEncoding v
    -- provideRepType typeActivityStreams2   $ return enc
    provideRepType typeActivityStreams2LD $ toEncoding <$> mk

provideAP' :: Monad m => m ByteString -> Writer (Endo [ProvidedRep m]) ()
provideAP' = provideRepType typeActivityStreams2LD

data APGetError
    = APGetErrorHTTP HttpException
    | APGetErrorJSON JSONException
    | APGetErrorContentType Text
    deriving Show

instance Exception APGetError

-- | Perform an HTTP GET request to fetch an ActivityPub object.
--
-- * Verify the URI scheme is _https:_ and authority part is present
-- * Set _Accept_ request header
-- * Perform the GET request
-- * Verify the _Content-Type_ response header
-- * Parse the JSON response body
httpGetAP
    :: (MonadIO m, UriMode u, FromJSON a)
    => Manager
    -> Either (ObjURI u) (SubURI u)
    -> m (Either APGetError (Response a))
httpGetAP manager uri =
    liftIO $
        mkResult <$> try (httpAPEither manager =<< requestFromURI (toURI uri))
    where
    toURI = either uriFromObjURI uriFromSubURI
    lookup' x = map snd . filter ((== x) . fst)
    mkResult (Left e)  = Left $ APGetErrorHTTP e
    mkResult (Right r) =
        case lookup' hContentType $ responseHeaders r of
            []  -> Left $ APGetErrorContentType "No Content-Type"
            [b] -> if b == typeActivityStreams2LD || b == typeActivityStreams2
                        then case responseBody r of
                            Left e  -> Left $ APGetErrorJSON e
                            Right v -> Right $ v <$ r
                        else Left $ APGetErrorContentType $ "Non-AP Content-Type: " <> decodeUtf8 b
            _   -> Left $ APGetErrorContentType "Multiple Content-Type"

data APPostError
    = APPostErrorSig S.HttpSigGenError
    | APPostErrorHTTP HttpException
    deriving Show

instance Exception APPostError

hActivityPubForwarder :: HeaderName
hActivityPubForwarder = "ActivityPub-Forwarder"

hForwardingSignature :: HeaderName
hForwardingSignature = "Forwarding-Signature"

hForwardedSignature :: HeaderName
hForwardedSignature = "Forwarded-Signature"

-- | Perform an HTTP POST request to submit an ActivityPub object.
--
-- * Verify the URI scheme is _https:_ and authority part is present
-- * Set _Content-Type_ request header
-- * Set _ActivityPub-Actor_ request header
-- * Set _Digest_ request header using SHA-256 hash
-- * If recipient is given, set _ActivityPub-Forwarder_ header and compute
--   _Forwarding-Signature_ header
-- * If forwarded signature is given, set set _ActivityPub-Forwarder_ and
--   _Forwarded-Signature_ headers
-- * Compute HTTP signature and add _Signature_ request header
-- * Perform the POST request
-- * Verify the response status is 2xx
httpPostAP
    :: (MonadIO m, UriMode u, ToJSON a)
    => Manager
    -> ObjURI u
    -> NonEmpty HeaderName
    -> S.KeyId
    -> (ByteString -> S.Signature)
    -> Text
    -> Maybe (Either (ObjURI u) ByteString)
    -> a
    -> m (Either APPostError (Response ()))
httpPostAP manager uri headers keyid sign uSender mfwd value =
    httpPostAPBytes manager uri headers keyid sign uSender mfwd $ encode value

-- | Like 'httpPostAP', except it takes the object as a raw lazy
-- 'BL.ByteString'. It's your responsibility to make sure it's valid JSON.
httpPostAPBytes
    :: (MonadIO m, UriMode u)
    => Manager
    -> ObjURI u
    -> NonEmpty HeaderName
    -> S.KeyId
    -> (ByteString -> S.Signature)
    -> Text
    -> Maybe (Either (ObjURI u) ByteString)
    -> BL.ByteString
    -> m (Either APPostError (Response ()))
httpPostAPBytes manager uri headers keyid sign uSender mfwd body =
    liftIO $ runExceptT $ do
        req <- requestFromURI $ uriFromObjURI uri
        let digest = formatHttpBodyDigest SHA256 "SHA-256" $ hashlazy body
            req' =
                setRequestCheckStatus $
                consHeader hContentType typeActivityStreams2LD $
                consHeader hActivityPubActor (encodeUtf8 uSender) $
                consHeader hDigest digest $
                req { method      = "POST"
                    , requestBody = RequestBodyLBS body
                    }
        req'' <- tryExceptT APPostErrorSig $ signRequest headers Nothing keyid sign Nothing req'
        req''' <-
            case mfwd of
                Nothing -> return req''
                Just (Left uRecip) ->
                    tryExceptT APPostErrorSig $
                        signRequestInto hForwardingSignature (hDigest :| [hActivityPubForwarder]) Nothing keyid sign Nothing $ consHeader hActivityPubForwarder (encodeUtf8 $ renderObjURI uRecip) req''
                Just (Right sig) ->
                    return $
                        consHeader hForwardedSignature sig $
                        consHeader hActivityPubForwarder (encodeUtf8 uSender)
                            req''
        tryExceptT APPostErrorHTTP $ httpNoBody req''' manager
    where
    consHeader n b r = r { requestHeaders = (n, b) : requestHeaders r }
    tryExceptT adapt action = ExceptT $ first adapt <$> try action

-- | Result of GETing the keyId URI and processing the JSON document.
data Fetched = Fetched
    { fetchedPublicKey      :: PublicVerifKey
      -- ^ The Ed25519 or RSA public key corresponding to the URI we requested.
    , fetchedKeyExpires     :: Maybe UTCTime
      -- ^ Optional expiration time declared for the key we received.
    , fetchedActorId        :: LocalURI
      -- ^ The @id URI of the actor for whom the key's signature applies.
    , fetchedActorName      :: Maybe Text
      -- ^ Name of the actor for whom the key's signature applies.
    , fetchedActorInbox     :: LocalURI
      -- ^ The inbox URI of the actor for whom the key's signature applies.
    , fetchedActorFollowers :: Maybe LocalURI
      -- ^ The follower collection URI of the actor for whom the key's
      -- signature applies.
    , fetchedKeyShared      :: Bool
      -- ^ Whether the key we received is shared. A shared key can sign
      --   requests for any actor on the same instance, while a personal key is
      --   only for one actor. Knowing whether the key is shared will allow us
      --   when receiving more requests, whether to accept signatures made on
      --   different actors, or allow only a single permanent actor for the key
      --   we received.
    }

fetchAP' :: (MonadIO m, UriMode u, FromJSON a) => Manager -> Either (ObjURI u) (SubURI u) -> ExceptT APGetError m a
fetchAP' m u = ExceptT $ second responseBody <$> httpGetAP m u

fetchAP :: (MonadIO m, UriMode u, FromJSON a) => Manager -> Either (ObjURI u) (SubURI u) -> ExceptT String m a
fetchAP m u = withExceptT displayException $ fetchAP' m u

{-
fetchAPH :: (MonadIO m, ActivityPub a) => Manager -> Text -> LocalURI -> ExceptT String m a
fetchAPH m h lu = do
    Doc h' v <- fetchAP m $ l2f h lu
    if h == h'
        then return v
        else throwE "Object @id URI's host doesn't match the URI we fetched"
-}

fetchAPID' :: (MonadIO m, UriMode u, ActivityPub a) => Manager -> (a u -> LocalURI) -> Authority u -> LocalURI -> m (Either (Maybe APGetError) (a u))
fetchAPID' m getId h lu = runExceptT $ do
    Doc h' v <- withExceptT Just $ fetchAP' m $ Left $ ObjURI h lu
    if h == h' && getId v == lu
        then return v
        else throwE Nothing

fetchRecipient :: (MonadIO m, UriMode u) => Manager -> Authority u -> LocalURI -> m (Either (Maybe APGetError) (Recipient u))
fetchRecipient m = fetchAPID' m getId
    where
    getId (RecipientActor a)      = actorId a
    getId (RecipientCollection c) = collectionId c

fetchAPID :: (MonadIO m, UriMode u, ActivityPub a) => Manager -> (a u -> LocalURI) -> Authority u -> LocalURI -> m (Either String (a u))
fetchAPID m getId h lu = first showError <$> fetchAPID' m getId h lu
    where
    showError Nothing  = "Object @id doesn't match the URI we fetched"
    showError (Just e) = displayException e

data FetchAPError
    = FetchAPErrorGet APGetError
    -- Object @id doesn't match the URI we fetched
    | FetchAPErrorIdMismatch
    -- Object @id URI's host doesn't match the URI we fetched
    | FetchAPErrorHostMismatch
    deriving Show

fetchAPIDOrH'
    :: (MonadIO m, UriMode u, ActivityPub a, ActivityPub b)
    => Manager
    -> (a u -> LocalRefURI)
    -> Authority u
    -> LocalRefURI
    -> ExceptT FetchAPError m (Either (a u) (b u))
fetchAPIDOrH' m getId h (LocalRefURI lu) = do
    e <- withExceptT FetchAPErrorGet $ fetchAP' m $ bimap (ObjURI h) (SubURI h) lu
    case e of
        Left' (Doc h' x) ->
            if h == h' && getId x == LocalRefURI lu
                then return $ Left x
                else throwE FetchAPErrorIdMismatch
        Right' (Doc h' y) ->
            if h == h'
                then return $ Right y
                else throwE FetchAPErrorHostMismatch

fetchAPIDOrH
    :: (MonadIO m, UriMode u, ActivityPub a, ActivityPub b)
    => Manager
    -> (a u -> LocalRefURI)
    -> Authority u
    -> LocalRefURI
    -> ExceptT String m (Either (a u) (b u))
fetchAPIDOrH m getId h lu = withExceptT show $ fetchAPIDOrH' m getId h lu

-- | Fetches the given actor and checks whether it lists the given key (as a
-- URI, not as an embedded object). If it does, returns 'Right' the fetched
-- actor. Otherwise, or if an error occurs during fetching, returns 'Left' an
-- error message.
keyListedByActor
    :: (MonadIO m, UriMode u)
    => Manager
    -> Authority u
    -> LocalRefURI
    -> LocalURI
    -> m (Either String (Actor u))
keyListedByActor manager host luKey luActor = runExceptT $ do
    actor <- ExceptT $ fetchAPID manager actorId host luActor
    if keyUriListed luKey actor
        then return actor
        else throwE "Actor publicKey has no URI matching pkey @id"
    where
    keyUriListed (LocalRefURI uk) a =
        let match (Left uri) = Left uri == uk
            match (Right _)  = False
        in  any match $ actorPublicKeys a

matchKeyObj
    :: (Foldable f, Monad m, UriMode u)
    => LocalRefURI
    -> f (Either LocalURI (PublicKey u))
    -> ExceptT String m (PublicKey u)
matchKeyObj luKey es =
    case find' (match luKey) es of
        Nothing -> throwE "keyId resolved to actor which doesn't have a key object with that ID"
        Just pk -> return pk
    where
    find' :: Foldable f => (a -> Maybe b) -> f a -> Maybe b
    find' p = join . fmap getFirst . foldMap (Just . First . p)
    match _   (Left _)   = Nothing
    match luk (Right pk) =
        if publicKeyId pk == luk
            then Just pk
            else Nothing

verifyAlgo :: Maybe S.Algorithm -> PublicVerifKey -> Either String ()
verifyAlgo Nothing  _ = Right ()
verifyAlgo (Just a) k =
    case a of
        S.AlgorithmEd25519 ->
            case k of
                PublicVerifKeyEd25519 _ -> Right ()
                PublicVerifKeyRSA _ ->
                    Left "Algo mismatch, algo is Ed25519 but actual key is RSA"
        S.AlgorithmRsaSha256 ->
            case k of
                PublicVerifKeyEd25519 _ ->
                    Left
                        "Algo mismatch, algo is RSA-SHA256 but actual key is \
                        \Ed25519"
                PublicVerifKeyRSA _ -> Right ()
        S.AlgorithmOther b -> Left $ concat
            [ "Unrecognized algo "
            , BC.unpack b
            , ", actual key is "
            , case k of
                PublicVerifKeyEd25519 _ -> "Ed25519"
                PublicVerifKeyRSA     _ -> "RSA"
            ]

-- | Fetch a key we don't have cached locally.
fetchUnknownKey
    :: (MonadIO m, UriMode u)
    => Manager
    -- ^ Manager for making HTTP requests
    -> Maybe S.Algorithm
    -- ^ Signature algorithm possibly specified in the HTTP signature header
    -> Authority u
    -- ^ Instance host
    -> Maybe LocalURI
    -- ^ Actor URI possibly provided in the HTTP request's actor header
    -> LocalRefURI
    -- ^ Key URI provided in HTTP signature header
    -> ExceptT String m Fetched
fetchUnknownKey manager malgo host mluActor luKey = do
    obj <- fetchAPIDOrH manager publicKeyId host luKey
    fetched <-
        case obj of
            Left pkey -> do
                (oi, luActor) <-
                    case publicKeyOwner pkey of
                        OwnerInstance ->
                            case mluActor of
                                Nothing -> throwE "Key is shared but actor header not specified!"
                                Just u  -> return (True, u)
                        OwnerActor owner -> do
                            for_ mluActor $ \ lu ->
                                if owner == lu
                                    then return ()
                                    else throwE "Key's owner doesn't match actor header"
                            return (False, owner)
                actor <- ExceptT $ keyListedByActor manager host luKey luActor
                return Fetched
                    { fetchedPublicKey      = publicKeyMaterial pkey
                    , fetchedKeyExpires     = publicKeyExpires pkey
                    , fetchedActorId        = luActor
                    , fetchedActorName      = actorName actor <|> actorUsername actor
                    , fetchedActorInbox     = actorInbox actor
                    , fetchedActorFollowers = actorFollowers actor
                    , fetchedKeyShared      = oi
                    }
            Right actor -> do
                case luKey of
                    LocalRefURI (Right lsu) |
                        actorId actor == localSubUriResource lsu -> return ()
                    _ -> throwE "Actor ID doesn't match the keyid URI we fetched"
                for_ mluActor $ \ lu ->
                    if actorId actor == lu
                        then return ()
                        else throwE "Key's owner doesn't match actor header"
                pk <- matchKeyObj luKey $ actorPublicKeys actor
                owner <- case publicKeyOwner pk of
                    OwnerInstance -> throwE "Actor's publicKey is shared, but embedded in actor document! We allow shared keys only if they're in a separate document"
                    OwnerActor owner ->
                        if owner == actorId actor
                            then return owner
                            else throwE "Actor's publicKey's owner doesn't match the actor's ID"
                return Fetched
                    { fetchedPublicKey      = publicKeyMaterial pk
                    , fetchedKeyExpires     = publicKeyExpires pk
                    , fetchedActorId        = owner
                    , fetchedActorName      = actorName actor <|> actorUsername actor
                    , fetchedActorInbox     = actorInbox actor
                    , fetchedActorFollowers = actorFollowers actor
                    , fetchedKeyShared      = False
                    }
    ExceptT . pure $ verifyAlgo malgo $ fetchedPublicKey fetched
    return fetched

keyDetail pk = (publicKeyMaterial pk, publicKeyExpires pk)

-- | Fetch a personal key we already have cached locally, but we'd like to
-- refresh the local copy by fetching the key again from the server.
fetchKnownPersonalKey
    :: (MonadIO m, UriMode u)
    => Manager
    -- ^ Manager for making HTTP requests
    -> Maybe S.Algorithm
    -- ^ Signature algorithm possibly specified in the HTTP signature header
    -> Authority u
    -- ^ Instance host
    -> LocalURI
    -- ^ Key owner actor ID URI
    -> LocalRefURI
    -- ^ Key URI
    -> ExceptT String m (PublicVerifKey, Maybe UTCTime)
fetchKnownPersonalKey manager malgo host luOwner luKey@(LocalRefURI ek) = do
    obj <- fetchAPIDOrH manager publicKeyId host luKey
    (material, mexpires) <-
        case obj of
            Left pkey -> do
                case publicKeyOwner pkey of
                    OwnerInstance -> throwE "Personal key became shared"
                    OwnerActor owner ->
                        when (luOwner /= owner) $ throwE "Key owner changed"
                return $ keyDetail pkey
            Right actor -> do
                unless (Right (actorId actor) == second localSubUriResource ek) $
                    throwE "Actor ID doesn't match the keyid URI we fetched"
                unless (actorId actor == luOwner) $
                    throwE "Key owner changed"
                pk <- matchKeyObj luKey $ actorPublicKeys actor
                case publicKeyOwner pk of
                    OwnerInstance -> throwE "Personal key became shared"
                    OwnerActor owner ->
                        when (owner /= luOwner) $
                            throwE "Actor's publicKey's owner doesn't match the actor's ID"
                return $ keyDetail pk
    ExceptT . pure $ verifyAlgo malgo material
    return (material, mexpires)

-- | Fetch a shared key we already have cached locally, but we'd like to
-- refresh the local copy by fetching the key again from the server.
fetchKnownSharedKey
    :: (MonadIO m, UriMode u)
    => Manager
    -- ^ Manager for making HTTP requests
    -> Maybe S.Algorithm
    -- ^ Signature algorithm possibly specified in the HTTP signature header
    -> Authority u
    -- ^ Instance host
    -> LocalURI
    -- ^ Actor ID from HTTP actor header
    -> LocalRefURI
    -- ^ Key URI
    -> ExceptT String m (PublicVerifKey, Maybe UTCTime)
fetchKnownSharedKey manager malgo host luActor luKey = do
    obj <- fetchAPIDOrH manager publicKeyId host luKey
    pkey <-
        case asKeyOrActor host obj of
            Left pk -> return pk
            Right _actor -> throwE "Expected stand-alone key, got embedded key"
    case publicKeyOwner pkey of
        OwnerInstance -> return ()
        OwnerActor _owner -> throwE "Shared key became personal"
    let (material, mexpires) = keyDetail pkey
    ExceptT . pure $ verifyAlgo malgo material
    return (material, mexpires)
    where
    asKeyOrActor
        :: Authority u
        -> Either (PublicKey u) (Actor u)
        -> Either (PublicKey u) (Actor u)
    asKeyOrActor _ = id

data Obj u = Obj
    { objId        :: ObjURI u
    , objType      :: Text

    , objContext   :: Maybe (ObjURI u)
    , objFollowers :: Maybe LocalURI
    , objInbox     :: Maybe LocalURI
    , objTeam      :: Maybe LocalURI
    }

instance UriMode u => FromJSON (Obj u) where
    parseJSON = withObject "Obj" $ \ o -> do
        id_@(ObjURI h _) <- o .: "id" <|> o .: "@id"
        Obj id_
            <$> (o .: "type" <|> o .: "@type")
            <*> o .:? "context"
            <*> withAuthorityMaybeO h (o .:? "followers")
            <*> withAuthorityMaybeO h (o .:? "inbox")
            <*> withAuthorityMaybeO h (o .:? "team")
