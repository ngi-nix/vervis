{- This file is part of Vervis.
 -
 - Written in 2019 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.ActorKey
    ( ActorKey ()
    , generateActorKey
    , actorKeyRotator
    , actorKeyPublicBin
    , actorKeySign
    -- , actorKeyVerify
    )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, modifyTVar')
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Crypto.Error (throwCryptoErrorIO)
import Crypto.PubKey.Ed25519 hiding (Signature)
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.Time.Interval (TimeInterval, microseconds)
import Data.PEM
import Data.X509
import Network.HTTP.Signature (Signature (..))
import System.Directory (doesFileExist)

import qualified Data.ByteString as B (writeFile, readFile)

import Crypto.PublicVerifKey
import Data.KeyFile

import Control.Concurrent.Local

-- | Ed25519 signing key, we generate it on the server and use for signing. We
-- also make its public key available to whoever wishes to verify our
-- signatures.
data ActorKey = ActorKey
    { actorKeySecret :: SecretKey
      -- ^ Secret key in binary form.
    , actorKeyPublic :: PublicKey
      -- ^ Public key in binary form.
    -- , actorKeyPubPEM :: ByteString
      -- ^ Public key in PEM format. This can be generated from the binary
      --   form, but we keep it here because it's used for sending the public
      --   key to whoever wishes to verify our signatures. So, we generate a
      --   key once and potentially send the PEM many times.
    }

instance KeyFile ActorKey where
    generateKey = generateActorKey
    parseKey b = do
        secret <- throwCryptoErrorIO $ secretKey b
        return ActorKey
            { actorKeySecret = secret
            , actorKeyPublic = toPublic secret
            }
    renderKey = convert . actorKeySecret

{-
-- | Ed25519 public key for signature verification. We receive these public
-- keys from other servers and we use them to verify HTTP request signatures.
data ActorPublicKey = ActorPublicKey
    { actorPublicKeyBin   :: PublicKey
      -- ^ Public key in binary form. This is used for signature verification.
    , actorPublicKeyPem   :: ByteString
      -- ^ Public key in PEM format. We can use it for formatting the key as
      --   JSON, and generally into textual formats.
    , actorPublicKeyId    :: URI
      -- ^ Public key ID URI. We can use it for formatting the key as JSON or
      --   other textual formats, and for verifying that it's identical to the
      --   URI we used for retrieving the key.
    , actorPublicKeyActor :: URI
      -- ^ Public key's actor URI. We can use it for formatting the key as JSON
      --   or other textual formats, and for verifying that it's identical to
      --   the actor ID through which we found the key. We can also check that
      --   this ID matches the actor ID to which content is attributed, to make
      --   sure we don't accept content claimed to be authored by someone other
      --   than the actor who signed the request.
    }

instance FromJSON ActorPublicKey where
    parseJSON = withObject "ActorPublicKey" $ \ o -> do
        pem <- o .: "publicKeyPem"
        ActorPublicKey
            <$> parsePEM pem
            <*> pure pem
            <*> parseURI' =<< (o .: "id" <|> o .: "@id")
            <*> parseURI' =<< o .: "owner"
    where
    parsePEM b =
        case pemParseBS b of
            Left e -> fail $ "PEM parsing failed: " ++ e
            Right xs ->
                case xs of
                    [] -> fail "Empty PEM"
                    [x] ->
                        case publickey $ pemContent x of
                            CryptoPassed k -> return k
                            CryptoFailed e -> fail $ show e
                    _ -> fail "Multiple PEM sections"
    parseURI' t =
        withText "URI" $ \ t ->
            case parseURI $ T.unpack t of
                Nothing -> fail "Invalid absolute URI"
                Just u ->
                    if uriScheme u == "https:"
                        then return u
                        else fail "URI scheme isn't https"

instance ToJSON ActorPublicKey where
    toJSON = error "toJSON ActorPublicKey"
    toEncoding (ActorPublicKey _ pem keyid actor) =
        pairs
            $  "id"           .= showURI keyid
            <> "owner"        .= showURI actor
            <> "publicKeyPem" .= pem
        where
        showURI u = uriToString id u ""
        {-
        array = Array . V.fromList
        context =
            array
                [ String "https://w3id.org/security/v1"
                , object [("id", String "@id")]
                ]
        -}
-}

-- | Generate a new random key.
generateActorKey :: IO ActorKey
generateActorKey = mk <$> generateSecretKey
    where
    mk secret =
        let public = toPublic secret
        in  ActorKey
                { actorKeySecret = secret
                , actorKeyPublic = public
                -- , actorKeyPubPEM = renderPEM public
                }
    -- renderPEM :: PublicKey -> ByteString
    -- renderPEM = pemWriteBS . PEM "PUBLIC KEY" [] . convert

-- | A loop that runs forever and periodically generates new actor keys,
-- storing them in a 'TVar'. It manages a pait of keys, and each time it toggles
-- which key gets rotated.
actorKeyRotator :: TimeInterval -> TVar (ActorKey, ActorKey, Bool) -> IO ()
actorKeyRotator interval keys = periodically interval $ do
    fresh <- generateActorKey
    atomically $
        modifyTVar' keys $ \ (k1, k2, new1) ->
            if new1
                then (k1   , fresh, False)
                else (fresh, k2   , True)

actorKeyPublicBin :: ActorKey -> PublicVerifKey
actorKeyPublicBin = fromEd25519 . actorKeyPublic

actorKeySign :: ActorKey -> ByteString -> Signature
actorKeySign (ActorKey sec pub) = Signature . convert . sign sec pub
