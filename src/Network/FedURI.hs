{- This file is part of Vervis.
 -
 - Written 2019 by fr33domlover <fr33domlover@riseup.net>.
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

{-# LANGUAGE DeriveGeneric #-}

module Network.FedURI
    ( Authority (..)
    , renderAuthority
    , LocalURI (..)
    , topLocalURI
    , LocalSubURI (..)
    , LocalPageURI (..)
    , LocalRefURI (..)
    , UriMode ()
    , Fed ()
    , Dev ()
    , ObjURI (..)
    , parseObjURI
    , uriFromObjURI
    , renderObjURI
    , SubURI (..)
    , uriFromSubURI
    , PageURI (..)
    , RefURI (..)
    , parseRefURI
    )
where

import Control.Monad
import Data.Aeson
import Data.Bifunctor
import Data.Char
import Data.Hashable
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding
import Data.Word
import Database.Persist.Class
import Database.Persist.Sql
import GHC.Generics (Generic)
import Text.Read
import Network.HTTP.Types.URI
import Network.URI hiding (scheme, path, query, fragment)

import qualified Data.Text as T

data Scheme = Plain | Secure deriving Eq

data Full

data Authority t = Authority
    { authorityHost :: Text
    , authorityPort :: Maybe Word16
    }
    deriving (Eq, Ord, Generic)

instance UriMode t => Hashable (Authority t)

parseAuthority :: UriMode t => Text -> Either String (Authority t)
parseAuthority t = do
    FullObjURI s a l <- toFullObjURI =<< parseFullURI ("https://" <> t)
    unless (s == Secure && l == topLocalURI) $
        Left "parseAuthority: Unexpected FullObjURI"
    let s' = case authorityPort a of
                Nothing -> Secure
                Just _ -> Plain
    checkAuthority s' a

renderAuthority :: Authority t -> Text
renderAuthority (Authority h Nothing)  = h
renderAuthority (Authority h (Just p)) = T.concat [h, ":", T.pack $ show p]

instance UriMode t => FromJSON (Authority t) where
    parseJSON = withText "Authority" $ either fail return . parseAuthority

instance UriMode t => ToJSON (Authority t) where
    toJSON = toJSON . renderAuthority
    toEncoding = toEncoding . renderAuthority

instance UriMode t => PersistField (Authority t) where
    toPersistValue = toPersistValue . renderAuthority
    fromPersistValue = first T.pack . parseAuthority <=< fromPersistValue

instance UriMode t => PersistFieldSql (Authority t) where
    sqlType = sqlType . fmap renderAuthority

data FullURI = FullURI
    { fullUriScheme    :: Scheme
    , fullUriAuthority :: Authority Full
    , fullUriPath      :: Text
    , fullUriQuery     :: Text
    , fullUriFragment  :: Text
    }

parseFullURI :: Text -> Either String FullURI
parseFullURI t = do
    uri <-
        case parseURI $ T.unpack t of
            Nothing -> Left "Invalid absolute URI"
            Just u -> Right u
    scheme <-
        case uriScheme uri of
            "http:" -> Right Plain
            "https:" -> Right Secure
            _ -> Left "URI scheme isn't http/s"
    URIAuth userInfo host port <-
        case uriAuthority uri of
            Nothing -> Left "URI has empty authority"
            Just a -> Right a
    unless (userInfo == "") $
        Left "URI has non-empty userinfo"
    portNumber <-
        case port of
            [] -> Right Nothing
            c:p ->
                case (c, readMaybe p) of
                    (':', Just n) ->
                        if n == 80 || n == 443
                            then Left "Unexpected port number"
                            else Right $ Just n
                    _ -> Left "Unexpected port number format"
    when (any (== ':') host) $
        Left "Host contains a colon"
    unless (any isAsciiLetter host) $
        Left "Host doesn't contain ASCII letters"
    Right FullURI
        { fullUriScheme    = scheme
        , fullUriAuthority = Authority
            { authorityHost = T.pack host
            , authorityPort = portNumber
            }
        , fullUriPath      = T.pack $ uriPath uri
        , fullUriQuery     = T.pack $ uriQuery uri
        , fullUriFragment  = T.pack $ uriFragment uri
        }
    where
    isAsciiLetter c = isAsciiLower c || isAsciiUpper c

fromFullURI :: FullURI -> URI
fromFullURI (FullURI scheme (Authority host mport) path query fragment) = URI
    { uriScheme    =
        case scheme of
            Plain -> "http:"
            Secure -> "https:"
    , uriAuthority = Just URIAuth
        { uriUserInfo = ""
        , uriRegName  = T.unpack host
        , uriPort     = maybe "" ((':' :) . show) mport
        }
    , uriPath      = T.unpack path
    , uriQuery     = T.unpack query
    , uriFragment  = T.unpack fragment
    }

renderFullURI :: FullURI -> Text
renderFullURI = T.pack . flip (uriToString id) "" . fromFullURI

instance FromJSON FullURI where
    parseJSON = withText "FullURI" $ either fail return . parseFullURI

instance ToJSON FullURI where
    toJSON = error "toJSON FullURI"
    toEncoding = toEncoding . renderFullURI

instance PersistField FullURI where
    toPersistValue = toPersistValue . renderFullURI
    fromPersistValue = first T.pack . parseFullURI <=< fromPersistValue

instance PersistFieldSql FullURI where
    sqlType = sqlType . fmap renderFullURI

data LocalURI = LocalURI
    { localUriPath :: Text
    }
    deriving (Eq, Ord, Generic)

instance Hashable LocalURI

dummyAuthority :: Authority Fed
dummyAuthority = Authority "h.h" Nothing

dummyPrefix :: Text
dummyPrefix = renderObjURI $ ObjURI dummyAuthority topLocalURI

instance PersistField LocalURI where
    toPersistValue = toPersistValue . renderLocalURI
        where
        renderLocalURI
            = fromJust
            . T.stripPrefix dummyPrefix
            . renderObjURI
            . ObjURI dummyAuthority
    fromPersistValue
        =   bimap T.pack objUriLocal . parseObjURI' . (dummyPrefix <>)
        <=< fromPersistValue
        where
        parseObjURI' :: Text -> Either String (ObjURI Fed)
        parseObjURI' = parseObjURI

instance PersistFieldSql LocalURI where
    sqlType = sqlType . fmap localUriPath

topLocalURI :: LocalURI
topLocalURI = LocalURI ""

data FullObjURI = FullObjURI
    { _fullObjUriScheme    :: Scheme
    , _fullObjUriAuthority :: Authority Full
    , _fullObjUriLocal     :: LocalURI
    }

toFullObjURI :: FullURI -> Either String FullObjURI
toFullObjURI (FullURI s a p q f) = do
    unless (q == "") $
        Left "URI query is non-empty"
    unless (f == "") $
        Left "URI fragment is non-empty"
    Right $ FullObjURI s a $ LocalURI p

fromFullObjURI :: FullObjURI -> FullURI
fromFullObjURI (FullObjURI s a (LocalURI p)) = FullURI s a p "" ""

instance FromJSON FullObjURI where
    parseJSON = either fail return . toFullObjURI <=< parseJSON

instance ToJSON FullObjURI where
    toJSON = toJSON . fromFullObjURI
    toEncoding = toEncoding . fromFullObjURI

instance PersistField FullObjURI where
    toPersistValue = toPersistValue . fromFullObjURI
    fromPersistValue = first T.pack . toFullObjURI <=< fromPersistValue

instance PersistFieldSql FullObjURI where
    sqlType = sqlType . fmap fromFullObjURI

data LocalSubURI = LocalSubURI
    { localSubUriResource :: LocalURI
    , localSubUriFragment :: Text
    }
    deriving (Eq, Generic)

instance Hashable LocalSubURI

instance PersistField LocalSubURI where
    toPersistValue = toPersistValue . renderLocalSubURI
        where
        renderLocalSubURI
            = fromJust
            . T.stripPrefix dummyPrefix
            . renderSubURI
            . SubURI dummyAuthority
            where
            renderSubURI :: UriMode t => SubURI t -> Text
            renderSubURI = renderFullURI . fromFullSubURI . fromSubURI
    fromPersistValue
        =   bimap T.pack subUriLocal . parseSubURI' . (dummyPrefix <>)
        <=< fromPersistValue
        where
        parseSubURI' :: Text -> Either String (SubURI Fed)
        parseSubURI' = parseSubURI
            where
            parseSubURI :: UriMode t => Text -> Either String (SubURI t)
            parseSubURI = toSubURI <=< toFullSubURI <=< parseFullURI

instance PersistFieldSql LocalSubURI where
    sqlType = sqlType . fmap localSubUriResource

data FullSubURI = FullSubURI
    { _fullSubUriScheme    :: Scheme
    , _fullSubUriAuthority :: Authority Full
    , _fullSubUriLocal     :: LocalSubURI
    }

toFullSubURI :: FullURI -> Either String FullSubURI
toFullSubURI (FullURI s a p q f) = do
    unless (T.null q) $
        Left "URI query is non-empty"
    case T.uncons f of
        Nothing -> Left "No URI fragment"
        Just ('#', f') ->
            when (T.null f') $
                Left "URI fragment is empty"
        _ -> Left "URI fragment unexpectedly doesn't start with a '#'"
    when (T.null f) $
        Left "URI fragment is empty"
    Right $ FullSubURI s a $ LocalSubURI (LocalURI p) f

fromFullSubURI :: FullSubURI -> FullURI
fromFullSubURI (FullSubURI s a (LocalSubURI (LocalURI p) f)) =
    FullURI s a p "" f

instance FromJSON FullSubURI where
    parseJSON = either fail return . toFullSubURI <=< parseJSON

instance ToJSON FullSubURI where
    toJSON = toJSON . fromFullSubURI
    toEncoding = toEncoding . fromFullSubURI

instance PersistField FullSubURI where
    toPersistValue = toPersistValue . fromFullSubURI
    fromPersistValue = first T.pack . toFullSubURI <=< fromPersistValue

instance PersistFieldSql FullSubURI where
    sqlType = sqlType . fmap fromFullSubURI

data LocalPageURI = LocalPageURI
    { localPageUriResource :: LocalURI
    , localPageUriParam    :: Text
    , localPageUriPage     :: Int
    }
    deriving (Eq, Generic)

instance Hashable LocalPageURI

data FullPageURI = FullPageURI
    { _fullPageUriScheme    :: Scheme
    , _fullPageUriAuthority :: Authority Full
    , _fullPageUriLocal     :: LocalPageURI
    }

toFullPageURI :: FullURI -> Either String FullPageURI
toFullPageURI (FullURI s a p q f) = do
    (param, mval) <-
        case parseQueryText $ encodeUtf8 q of
            [] -> Left "URI query is empty"
            [qp] -> Right qp
            _ -> Left "URI has multiple query parameters"
    val <-
        case mval of
            Nothing -> Left "URI query parameter doesn't have a value"
            Just v -> Right v
    page <-
        case readMaybe $ T.unpack val of
            Nothing -> Left "URI query param value isn't an integer"
            Just n -> Right n
    unless (page >= 1) $
        Left "URI page number isn't positive"
    unless (f == "") $
        Left "URI fragment is non-empty"
    Right $ FullPageURI s a $ LocalPageURI (LocalURI p) param page

fromFullPageURI :: FullPageURI -> FullURI
fromFullPageURI (FullPageURI s a (LocalPageURI (LocalURI p) param page)) =
    FullURI s a p q ""
    where
    q = T.concat ["?", param, "=", T.pack $ show page]

instance FromJSON FullPageURI where
    parseJSON = either fail return . toFullPageURI <=< parseJSON

instance ToJSON FullPageURI where
    toJSON = toJSON . fromFullPageURI
    toEncoding = toEncoding . fromFullPageURI

instance PersistField FullPageURI where
    toPersistValue = toPersistValue . fromFullPageURI
    fromPersistValue = first T.pack . toFullPageURI <=< fromPersistValue

instance PersistFieldSql FullPageURI where
    sqlType = sqlType . fmap fromFullPageURI

newtype LocalRefURI = LocalRefURI (Either LocalURI LocalSubURI)
    deriving (Eq, Generic)

instance Hashable LocalRefURI

instance PersistField LocalRefURI where
    toPersistValue (LocalRefURI u) = either toPersistValue toPersistValue u
    fromPersistValue v =
        LocalRefURI <$>
            aor (Left <$> fromPersistValue v) (Right <$> fromPersistValue v)
        where
        aor :: Either a b -> Either a b -> Either a b
        aor (Left _)    y = y
        aor a@(Right _) _ = a

instance PersistFieldSql LocalRefURI where
    sqlType = sqlType . fmap f
        where
        f (LocalRefURI u) = either id localSubUriResource u

data FullRefURI = FullRefURI
    { _fullRefUriScheme    :: Scheme
    , _fullRefUriAuthority :: Authority Full
    , _fullRefUriLocal     :: LocalRefURI
    }

toFullRefURI :: FullURI -> Either String FullRefURI
toFullRefURI fu =
    case toFullObjURI fu of
        Left _ -> sub2ref <$> toFullSubURI fu
        Right ou -> Right $ obj2ref ou
    where
    obj2ref (FullObjURI s a l) = FullRefURI s a $ LocalRefURI $ Left l
    sub2ref (FullSubURI s a l) = FullRefURI s a $ LocalRefURI $ Right l

fromFullRefURI :: FullRefURI -> FullURI
fromFullRefURI (FullRefURI s a (LocalRefURI e)) =
    case e of
        Left l -> fromFullObjURI $ FullObjURI s a l
        Right l -> fromFullSubURI $ FullSubURI s a l

instance FromJSON FullRefURI where
    parseJSON = either fail return . toFullRefURI <=< parseJSON

instance ToJSON FullRefURI where
    toJSON = toJSON . fromFullRefURI
    toEncoding = toEncoding . fromFullRefURI

instance PersistField FullRefURI where
    toPersistValue = toPersistValue . fromFullRefURI
    fromPersistValue = first T.pack . toFullRefURI <=< fromPersistValue

instance PersistFieldSql FullRefURI where
    sqlType = sqlType . fmap fromFullRefURI

class UriMode a where
    checkAuthority  :: Scheme -> Authority Full -> Either String (Authority a)
    authorityScheme :: Authority a -> Scheme

toFull :: UriMode a => Authority a -> Authority Full
toFull (Authority h mp) = Authority h mp

data Fed

instance UriMode Fed where
    checkAuthority s (Authority h mp)
        | s /= Secure      = Left "Scheme isn't HTTPS"
        | isJust mp        = Left "Port number present"
        | T.all (/= '.') h = Left "Host doesn't contain periods"
        | otherwise        = Right $ Authority h mp
    authorityScheme _ = Secure

data Dev

instance UriMode Dev where
    checkAuthority s (Authority h mp)
        | s /= Plain       = Left "Scheme isn't HTTP"
        | isNothing mp     = Left "Port number missing"
        | T.any (== '.') h = Left "Host contains periods"
        | otherwise        = Right $ Authority h mp
    authorityScheme _ = Plain

data ObjURI t = ObjURI
    { objUriAuthority :: Authority t
    , objUriLocal     :: LocalURI
    }
    deriving (Eq, Generic)

instance UriMode t => Hashable (ObjURI t)

toObjURI :: UriMode t => FullObjURI -> Either String (ObjURI t)
toObjURI (FullObjURI s a l) = flip ObjURI l <$> checkAuthority s a

fromObjURI :: UriMode t => ObjURI t -> FullObjURI
fromObjURI (ObjURI a l) = FullObjURI (authorityScheme a) (toFull a) l

parseObjURI :: UriMode t => Text -> Either String (ObjURI t)
parseObjURI = toObjURI <=< toFullObjURI <=< parseFullURI

uriFromObjURI :: UriMode t => ObjURI t -> URI
uriFromObjURI = fromFullURI . fromFullObjURI . fromObjURI

renderObjURI :: UriMode t => ObjURI t -> Text
renderObjURI = renderFullURI . fromFullObjURI . fromObjURI

instance UriMode t => FromJSON (ObjURI t) where
    parseJSON = either fail return . toObjURI <=< parseJSON

instance UriMode t => ToJSON (ObjURI t) where
    toJSON = toJSON . fromObjURI
    toEncoding = toEncoding . fromObjURI

instance UriMode t => PersistField (ObjURI t) where
    toPersistValue = toPersistValue . fromObjURI
    fromPersistValue = first T.pack . toObjURI <=< fromPersistValue

instance UriMode t => PersistFieldSql (ObjURI t) where
    sqlType = sqlType . fmap fromObjURI

data SubURI t = SubURI
    { subUriAuthority :: Authority t
    , subUriLocal     :: LocalSubURI
    }
    deriving (Eq, Generic)

instance UriMode t => Hashable (SubURI t)

toSubURI :: UriMode t => FullSubURI -> Either String (SubURI t)
toSubURI (FullSubURI s a l) = flip SubURI l <$> checkAuthority s a

fromSubURI :: UriMode t => SubURI t -> FullSubURI
fromSubURI (SubURI a l) = FullSubURI (authorityScheme a) (toFull a) l

uriFromSubURI :: UriMode t => SubURI t -> URI
uriFromSubURI = fromFullURI . fromFullSubURI . fromSubURI

instance UriMode t => FromJSON (SubURI t) where
    parseJSON = either fail return . toSubURI <=< parseJSON

instance UriMode t => ToJSON (SubURI t) where
    toJSON = toJSON . fromSubURI
    toEncoding = toEncoding . fromSubURI

instance UriMode t => PersistField (SubURI t) where
    toPersistValue = toPersistValue . fromSubURI
    fromPersistValue = first T.pack . toSubURI <=< fromPersistValue

instance UriMode t => PersistFieldSql (SubURI t) where
    sqlType = sqlType . fmap fromSubURI

data PageURI t = PageURI
    { pageUriAuthority :: Authority t
    , pageUriLocal     :: LocalPageURI
    }
    deriving (Eq, Generic)

instance UriMode t => Hashable (PageURI t)

toPageURI :: UriMode t => FullPageURI -> Either String (PageURI t)
toPageURI (FullPageURI s a l) = flip PageURI l <$> checkAuthority s a

fromPageURI :: UriMode t => PageURI t -> FullPageURI
fromPageURI (PageURI a l) = FullPageURI (authorityScheme a) (toFull a) l

instance UriMode t => FromJSON (PageURI t) where
    parseJSON = either fail return . toPageURI <=< parseJSON

instance UriMode t => ToJSON (PageURI t) where
    toJSON = toJSON . fromPageURI
    toEncoding = toEncoding . fromPageURI

instance UriMode t => PersistField (PageURI t) where
    toPersistValue = toPersistValue . fromPageURI
    fromPersistValue = first T.pack . toPageURI <=< fromPersistValue

instance UriMode t => PersistFieldSql (PageURI t) where
    sqlType = sqlType . fmap fromPageURI

data RefURI t = RefURI
    { refUriAuthority :: Authority t
    , refUriLocal     :: LocalRefURI
    }
    deriving (Eq, Generic)

instance UriMode t => Hashable (RefURI t)

toRefURI :: UriMode t => FullRefURI -> Either String (RefURI t)
toRefURI (FullRefURI s a l) = flip RefURI l <$> checkAuthority s a

fromRefURI :: UriMode t => RefURI t -> FullRefURI
fromRefURI (RefURI a l) = FullRefURI (authorityScheme a) (toFull a) l

parseRefURI :: UriMode t => Text -> Either String (RefURI t)
parseRefURI = toRefURI <=< toFullRefURI <=< parseFullURI

uriFromRefURI :: UriMode t => RefURI t -> URI
uriFromRefURI = fromFullURI . fromFullRefURI . fromRefURI

instance UriMode t => FromJSON (RefURI t) where
    parseJSON = either fail return . toRefURI <=< parseJSON

instance UriMode t => ToJSON (RefURI t) where
    toJSON = toJSON . fromRefURI
    toEncoding = toEncoding . fromRefURI

instance UriMode t => PersistField (RefURI t) where
    toPersistValue = toPersistValue . fromRefURI
    fromPersistValue = first T.pack . toRefURI <=< fromPersistValue

instance UriMode t => PersistFieldSql (RefURI t) where
    sqlType = sqlType . fmap fromRefURI
