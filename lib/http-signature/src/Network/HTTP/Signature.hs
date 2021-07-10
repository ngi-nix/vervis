{- This file is part of http-signature.
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

{-# LANGUAGE OverloadedStrings #-}

-- TODO also implement:
--
-- * digest hashing, support SHA-256 and SKEIN-512 to get started?
--
-- The HTTP sigs draft mentions the Digest header thing, it's from RFC 3230. I
-- couldn't find any Haskell implementation though. Thoughts:
--
-- (1) Implement that thing in a separate library
-- (2) Make it possible to use it in http-signature, but optional. That's
--     because it's possible your web app already verifies Digest headers, so
--     we don't need to do it again ourselves. We can just use the Digest
--     header as-is in the signature, because other code already verified the
--     hash matches the actual request body.

module Network.HTTP.Signature
    ( Request (..)
    , Algorithm (..)
    , KeyId (..)
    , Signature (..)
    , Verification (..)
    , HttpSigGenError ()
    , HttpSigVerError ()
    , hRequestTarget
    , hSignature
    , signRequest
    , prepareToVerify
    , prepareToVerifyWith
    )
where

import Control.Exception (Exception)
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (CI)
import Data.Foldable
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty, groupAllWith1)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime, diffUTCTime)
import Network.HTTP.Date (httpDateToUTC, parseHTTPDate)
import Network.HTTP.Types.Header (HeaderName, RequestHeaders, hDate)

import qualified Data.ByteString as B (null, concat, intercalate)
import qualified Data.ByteString.Base64 as B64 (encode, decode)
import qualified Data.ByteString.Char8 as BC (dropWhile, spanEnd, split)
import qualified Data.CaseInsensitive as CI (mk, foldedCase)
import qualified Data.List.NonEmpty as NE (toList)

data Request = Request
    { requestMethod  :: CI ByteString
    , requestPath    :: ByteString
    , requestHeaders :: RequestHeaders
    }

data Algorithm
    = AlgorithmEd25519
    | AlgorithmRsaSha256
    | AlgorithmOther ByteString

newtype KeyId = KeyId
    { unKeyId :: ByteString
    }
    deriving Eq

newtype Signature = Signature
    { unSignature :: ByteString
    }

data Verification = Verification
    { verAlgorithm :: Maybe Algorithm
    , verKeyId     :: KeyId
    , verInput     :: ByteString
    , verSignature :: Signature
    }

data HttpSigGenError
    = MethodEmpty
    | PathEmpty
    | HeaderDuplicate HeaderName
    | HeaderNotFound HeaderName
    | AlgoEmpty
    | KeyIdEmpty
    | SigEmpty
    deriving Show

instance Exception HttpSigGenError

data HttpSigVerError
    = SigHeaderMissing
    | SigHeaderDuplicated
    | SigHeaderParsingFailed String
    | UnknownParam ByteString
    | AlgoDuplicated
    | KeyIdMissing
    | KeyIdDuplicated
    | HeadersDuplicated
    | HeadersExtraSpace
    | DateHeaderNotUsed
    | DateHeaderDuplicated
    | DateHeaderMissing
    | DateHeaderParsingFailed ByteString
    | DateRejected UTCTime
    | RequiredHeaderNotUsed HeaderName
    | WantedHeaderNotUsed HeaderName
    | SigMissing
    | SigDuplicated
    | SigInvalidBase64 String
    | InputGenFailed HttpSigGenError
    deriving Show

instance Exception HttpSigVerError

hRequestTarget :: HeaderName
hRequestTarget = "(request-target)"

hSignature :: HeaderName
hSignature = "Signature"

lookupParam :: Eq a => a -> [(a, b)] -> [b]
lookupParam n = map snd . filter ((== n) . fst)

paramElem :: Eq a => a -> [(a, b)] -> Bool
paramElem x l = elem x $ map fst l

detectDuplicateHeaders
    :: NonEmpty HeaderName -> Either HttpSigGenError (NonEmpty HeaderName)
detectDuplicateHeaders hs =
    case find nonSingleton $ groupAllWith1 CI.foldedCase hs of
        Just (h :| _) -> Left $ HeaderDuplicate h
        Nothing       -> Right hs
    where
    nonSingleton (_ :| []) = False
    nonSingleton _         = True

renderInput
    :: Request -> NonEmpty HeaderName -> Either HttpSigGenError ByteString
renderInput request names = do
    requestTarget <- renderTarget request
    fields <-
        traverse (renderHeader (requestHeaders request) requestTarget) names
    Right $ B.intercalate "\n" $ NE.toList fields
  where
    renderTarget (Request m p _)
        | B.null m' = Left MethodEmpty
        | B.null p  = Left PathEmpty
        | otherwise = Right $ B.concat [m', " ", p]
        where
        m' = CI.foldedCase m
    renderHeader headers target name =
        if name == hRequestTarget
            then Right $ CI.foldedCase name <> ": " <> target
            else case lookupParam name headers of
                [] -> Left $ HeaderNotFound name
                values ->
                    Right $
                    B.concat
                        [ CI.foldedCase name
                        , ": "
                        , B.intercalate ", " $ map stripSpace values
                        ]
        where
        stripSpace = dropWhileEnd isSpace . BC.dropWhile isSpace
            where
            dropWhileEnd p = fst . BC.spanEnd p

-- | Sign an HTTP request and return the value to use in the @\"Signature\"@
-- HTTP request header.
signRequest
    :: NonEmpty HeaderName
    -- ^ List of HTTP headers to include in the signed string. The special
    --   'HeaderTarget' value will be produced from the request method, path
    --   and query string. The default list specified by the spec is just the
    --   @\"Date\"@ header.
    -> Maybe Algorithm
    -- ^ Optionally specify the algorithm used for the signature.
    --
    --   The spec says it's preferred that the entity verifying the signature
    --   derives the algorithm from the key, but if you're communicating with
    --   an entity which fails without the algorithm, you may wish to provide
    --   it.
    -> KeyId
    -- ^ The ID of the key used for the signature.
    -> (ByteString -> Signature)
    -- ^ Signing function. Given a string to sign, it's supposed to produce a
    --   cryptographic signature and provide it in binary form, using the key
    --   specified by the previous parameter.
    -> Request
    -- ^ Details of the HTTP request. The input for the signature is produced
    --   from them.
    -> Either HttpSigGenError ByteString
    -- ^ Formatted value for @\"Signature\"@ header, ready for use in an HTTP
    --   request.
signRequest headers malgo keyid sign request = do
    headers' <- detectDuplicateHeaders headers
    input <- renderInput request headers'
    renderSig headers' malgo keyid $ sign input
  where
    renderSig ns ma (KeyId k) (Signature s)
        | maybe False algoNull ma = Left AlgoEmpty
        | B.null k                = Left KeyIdEmpty
        | B.null s                = Left SigEmpty
        | otherwise               =
            Right $ B.concat $ maybe id ((:) . renderAlgo) ma $
                [ "keyId=\"", k
                , "\",headers=\"", renderNames ns
                , "\",signature=\"", B64.encode s, "\""
                ]
        where
        algoNull (AlgorithmOther b) = B.null b
        algoNull _                  = False
        renderAlgo AlgorithmEd25519   = "ed25519"
        renderAlgo AlgorithmRsaSha256 = "rsa-sha256"
        renderAlgo (AlgorithmOther b) = b
        renderNames = B.intercalate " " . map CI.foldedCase . NE.toList

-- | Parse the _Signature_ header and compute the signature input string.
prepareToVerify
    :: [HeaderName]
    -> [HeaderName]
    -> Int
    -> UTCTime
    -> Request
    -> Either HttpSigVerError Verification
prepareToVerify requires wants seconds now =
    prepareToVerifyWith hSignature True requires wants $ Just (seconds, now)

-- | Like 'prepareToVerify', except the parameters that provide more
-- flexibility:
--
-- * The first parameter allows to specify in which HTTP header the signature
--   is found. In 'prepareToVerify', the standard _Signature_ header is used.
--
-- * The second parameter specifies whether the _Date_ should be required to be
--   used in the signature, even if not specified in the list of required
--   headers (i.e. the 3rd parameter). In 'prepareToVerify', this is set to
--   'True'.
--
-- * The 5th parameter controls whether to compare the _Date_ header with the
--   current time and require the difference between them to be within the
--   given margin (specified in seconds). 'Nothing' means don't check.
--   @Just (seconds, now)@ means that the _Date_ must exist, and require it
--   its difference from the current time, given by @now@, to be at most
--   @seconds@.
prepareToVerifyWith
    :: HeaderName
    -> Bool
    -> [HeaderName]
    -> [HeaderName]
    -> Maybe (Int, UTCTime)
    -> Request
    -> Either HttpSigVerError Verification
prepareToVerifyWith hSig requireDate requires wants checkDate request = do
    let headers = requestHeaders request
    sigHdr <- findSigHeader headers
    (malgo, keyid, names, signature) <- parseSigHeader sigHdr
    unless (not requireDate || hDate `elem` names) $
        Left DateHeaderNotUsed
    traverse_ (requireUsage names) requires
    traverse_ (wantUsage headers names) wants
    for_ checkDate $ \ (seconds, now) -> do
        dateHdr <- findDateHeader headers
        date <- parseDateHeader dateHdr
        let diff = now `diffUTCTime` date
        unless (0 <= diff && diff <= fromIntegral seconds) $
            Left $ DateRejected date
    input <- first InputGenFailed $ renderInput request names
    Right $ Verification malgo keyid input signature
  where
    requireUsage names required =
        if required `elem` names
            then Right ()
            else Left $ RequiredHeaderNotUsed required
    wantUsage headers names wanted =
        if wanted `notElem` names || wanted `paramElem` headers
            then Right ()
            else Left $ WantedHeaderNotUsed wanted
    findSigHeader hs =
        case lookupParam hSig hs of
            []  -> Left SigHeaderMissing
            [h] -> Right h
            _   -> Left SigHeaderDuplicated
    findDateHeader hs =
        case lookupParam hDate hs of
            []  -> Left DateHeaderMissing
            [h] -> Right h
            _   -> Left DateHeaderDuplicated
    parseSigHeader b = do
        ps <- first SigHeaderParsingFailed $ parseOnly (params <* endOfInput) b
        (a, k, h, s) <- foldlM collect (Nothing, Nothing, Nothing, Nothing) ps
        k' <- maybe (Left KeyIdMissing) Right k
        s' <- maybe (Left SigMissing) Right s
        let h' = fromMaybe (hDate :| []) h
        Right (a, k', h', s')
      where
        valueChar c = ' ' <= c && c <= '~' && c /= '"' && c /= '\\'
        paramName = takeWhile1 isAlpha_ascii
        paramValue = takeWhile1 valueChar
        param = (,) <$> paramName <* "=\"" <*> paramValue <* char '"'
        params = param `sepBy` char ','
        parseAlgo "ed25519"    = AlgorithmEd25519
        parseAlgo "rsa-sha256" = AlgorithmRsaSha256
        parseAlgo a            = AlgorithmOther a
        parseName n =
            if B.null n
                then Left HeadersExtraSpace
                else Right $ CI.mk n
        collect (ma, mk, mh, ms) (n, v) =
            case n of
                "algorithm" ->
                    case ma of
                        Just _ -> Left AlgoDuplicated
                        Nothing -> Right (Just $ parseAlgo v, mk, mh, ms)
                "keyId" ->
                    case mk of
                        Just _ -> Left KeyIdDuplicated
                        Nothing -> Right (ma, Just $ KeyId v, mh, ms)
                "headers" ->
                    case mh of
                        Just _ -> Left HeadersDuplicated
                        Nothing -> do
                            ns <- traverse parseName $ BC.split ' ' v
                            case nonEmpty ns of
                                Nothing ->
                                    Left $ SigHeaderParsingFailed "No headers"
                                Just ns' -> do
                                    ns'' <-
                                        first InputGenFailed $
                                            detectDuplicateHeaders ns'
                                    Right (ma, mk, Just ns'', ms)
                "signature" ->
                    case ms of
                        Just _ -> Left SigDuplicated
                        Nothing ->
                            case B64.decode v of
                                Left e -> Left $ SigInvalidBase64 e
                                Right s ->
                                    Right (ma, mk, mh, Just $ Signature s)
                _ -> Left $ UnknownParam n
    parseDateHeader b =
        case parseHTTPDate b of
            Nothing -> Left $ DateHeaderParsingFailed b
            Just d  -> Right $ httpDateToUTC d
