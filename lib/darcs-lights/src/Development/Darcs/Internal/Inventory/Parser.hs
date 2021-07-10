{- This file is part of darcs-lights.
 -
 - Written in 2016, 2018 by fr33domlover <fr33domlover@riseup.net>.
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

-- We use the ByteString based Attoparsec and not the Text based one because we
-- need to create a hash of the patch info. If we use the Text one, Attoparsec
-- decodes the text, hopefully as UTF-8, and then we need to encode again to
-- ByteString for the hashing. This is dangerous because if the encoding
-- doesn't result with the exact original text, we'll have the wrong hash. To
-- make sure it's exactly the right content, we use ByteString first and then
-- later decode to Text.
module Development.Darcs.Internal.Inventory.Parser
    ( latestInventoryPristineP
    , latestInventorySizeP
    , latestInventoryPrevSizeP
    , latestInventoryPageP
    , latestInventoryAllP
    , earlyInventorySizeP
    , earlyInventoryPrevSizeP
    , earlyInventoryPageP
    , earlyInventoryAllP
    , patchInfoRawP'
    )
where

import Prelude hiding (take, takeWhile)

import Control.Applicative (many, optional, liftA2)
import Control.Arrow (second)
import Control.Monad (replicateM_)
import Crypto.Hash
import Data.Attoparsec.ByteString
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.Time.Calendar (fromGregorianValid)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import Data.Word (Word8)

import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lex.Integral as BX

import Control.Applicative.Local
import Development.Darcs.Internal.Hash.Types
import Development.Darcs.Internal.Inventory.Types
import Development.Darcs.Internal.Patch
import Development.Darcs.Internal.Patch.Types

lf       :: Word8
lf       = 10
space    :: Word8
space    = 32
star     :: Word8
star     = 42
dash     :: Word8
dash     = 45
zero     :: Word8
zero     = 48
nine     :: Word8
nine     = 57
sqrOpen  :: Word8
sqrOpen  = 91
--sqrClose :: Word8
--sqrClose = 93

digit :: Parser Word8
digit = satisfy $ \ w -> zero <= w && w <= nine

digitP :: Num a => Parser a
digitP = fmap (\ c -> fromIntegral $ c - zero) digit

decimal2P :: Num a => Parser a
decimal2P =
    (\ h l -> 10 * h + l) <$>
    digitP <*>
    digitP

decimal4P :: Num a => Parser a
decimal4P =
    (\ hh h l ll -> 10 * (10 * (10 * hh + h) + l) + ll) <$>
    digitP <*>
    digitP <*>
    digitP <*>
    digitP

patchTimeP :: Parser UTCTime
patchTimeP = do
    year <- decimal4P
    month <- decimal2P
    day <- decimal2P

    hours <- decimal2P
    minutes <- decimal2P
    seconds <- decimal2P

    case fromGregorianValid year month day of
        Nothing -> fail "Invalid patch date"
        Just uday -> return UTCTime
            { utctDay     = uday
            , utctDayTime =
                secondsToDiffTime $ 3600 * hours + 60 * minutes + seconds
            }

_line :: Parser ByteString
_line = _restOfLine

_restOfLine :: Parser ByteString
_restOfLine = takeWhile (/= lf)

eol :: Parser ()
eol = skip (== lf)

skipLine :: Parser ()
skipLine = skipWhile (/= lf)

skipRestOfLine :: Parser ()
skipRestOfLine = skipLine

skipPatchP :: Parser ()
skipPatchP =
    -- title
    skipLine *> eol *>
    -- author, inverted, time
    skipLine *> eol *>
    -- ignore, description
    (skipMany $ skip (== space) *> skipRestOfLine *> eol) *>
    -- end of info
    (string "] \n") *>
    -- hash
    skipLine

sha256P :: Parser ByteString
sha256P = do
    bs <- take 64
    case second B.null $ B16.decode bs of
        (h, True) -> return h
        _         -> fail "SHA256 decoding from hex failed"

sizeP :: Parser Int
sizeP = do
    bs <- take 10
    case second B.null <$> BX.readDecimal bs of
        Just (n, True) -> return n
        _              -> fail "sizeP failed"

sizeSha256P :: Parser (Int, ByteString)
sizeSha256P = liftA2 (,) sizeP (skip (== dash) *> sha256P)

pristineP :: Parser PristineHash
pristineP = string "pristine:" *> (PristineHash <$> sha256P)

prevInvP :: Parser InventoryHash
prevInvP =
    string "Starting with inventory" *> eol *>
    (uncurry InventoryHash <$> sizeSha256P)

patchInfoRawP' :: Parser PatchInfoRaw
patchInfoRawP' = do
    _ <- word8 sqrOpen
    title <- takeWhile1 (/= lf)
    eol

    author <- takeWhile1 (/= star)
    _ <- word8 star
    inverted <- (/= star) <$> (satisfy $ \ c -> c == star || c == dash)
    (timeRaw, time) <- match patchTimeP
    eol

    _ <- word8 space
    junkp <- string "Ignore-this: "
    junkc <- takeWhile1 (/= lf)
    eol

    descLines <- many $ word8 space *> takeWhile (/= lf) <* eol
    _ <- string "] "

    return PatchInfoRaw
        { pirAuthor      = author
        , pirTitle       = title
        , pirDescription = descLines
        , pirJunkPrefix  = junkp
        , pirJunkContent = junkc
        , pirTime        = (timeRaw, time)
        , pirInverted    = inverted
        }

patchInfoRawP :: Parser (PatchInfoRaw, (Int, ByteString))
patchInfoRawP = do
    pir <- patchInfoRawP'
    _ <- string "\nhash: "
    contentHash <- sizeSha256P
    return (pir, contentHash)

-- TODO
--
-- * Finish DarcsRev code, make it build
-- * Update darcs change view code to work correctly in the case of previous
--   inventories, test vervis against libravatar for that

-- | Parse patch metadata and compute the metadata's hash, which can be used as
-- a patch identifier for lookup and matching.
patchInfoP :: Parser (PatchInfo, PatchInfoHash, PatchContentHash)
patchInfoP = do
    (pir, contentHash) <- patchInfoRawP
    let (pinfo, chash) = refinePatchInfo pir contentHash
    return (pinfo, PatchInfoHash $ convert $ hashPatchInfo SHA1 pir, chash)

tagInfoP :: Parser (TagInfo, PatchInfoHash, PatchContentHash)
tagInfoP = do
    (pinfo, pih, pch) <- patchInfoP
    case patchToTag pinfo of
        Nothing -> fail "Expected a tag, got a patch that isn't a tag"
        Just ti -> return (ti, pih, pch)

-------------------------------------------------------------------------------
-- Latest inventory
-------------------------------------------------------------------------------

latestInventoryPristineP :: Parser PristineHash
latestInventoryPristineP = pristineP

latestInventorySizeP :: Parser Int
latestInventorySizeP =
    -- pristine hash
    skipLine *>
    -- previous inventory
    optional
        ( eol *> string "Starting" *> skipRestOfLine *>
          eol *> skipLine
        ) *>
    -- patch info
    (length <$> many (eol *> skipPatchP)) <*
    eol

latestInventoryPrevSizeP :: Parser (Maybe InventoryHash, Int)
latestInventoryPrevSizeP =
    liftA2 (,)
        ( -- pristine hash
          skipLine *>
          -- previous inventory
          optional (eol *> prevInvP)
        )
        ( -- patch info
          (length <$> many (eol *> skipPatchP)) <*
          eol
        )

latestInventoryPageP
    :: Int -> Int -> Parser (Maybe InventoryHash, [(PatchInfo, PatchInfoHash, PatchContentHash)])
latestInventoryPageP off lim =
    let f mPrevTag pis =
            case mPrevTag of
                Nothing      -> (Nothing, pis)
                Just (ih, p) -> (Just ih, p : pis)
    in  liftA2 f
              -- pristine
            ( skipLine *>
              -- previous inventory and clean tag
              optional (liftA2 (,) (eol *> prevInvP) (eol *> patchInfoP)) <*
              -- skip offset
              replicateM_ off (eol *> skipPatchP)
            )
            -- take limit
            (atMost lim $ eol *> patchInfoP)

latestInventoryAllP :: Parser LatestInventory
latestInventoryAllP = LatestInventory
    <$> pristineP
    <*> optional (liftA2 (,) (eol *> prevInvP) (eol *> tagInfoP))
    <*> many (eol *> patchInfoP)
    <*  eol

-------------------------------------------------------------------------------
-- Early inventory
-------------------------------------------------------------------------------

earlyInventorySizeP :: Parser Int
earlyInventorySizeP =
    -- previous inventory
    optional
        ( string "Starting" *> skipRestOfLine *>
          eol *> skipLine
        ) *>
    -- patch info
    (length <$> many (eol *> skipPatchP)) <*
    eol

earlyInventoryPrevSizeP :: Parser (Maybe InventoryHash, Int)
earlyInventoryPrevSizeP =
    liftA2 (,)
        -- previous inventory
        (optional $ prevInvP <* eol)
        -- patch info
        (length <$> many (skipPatchP *> eol))

earlyInventoryPageP
    :: Int -> Int -> Parser (Maybe InventoryHash, [(PatchInfo, PatchInfoHash, PatchContentHash)])
earlyInventoryPageP off lim =
    let f mPrevTag pis =
            case mPrevTag of
                Nothing      -> (Nothing, pis)
                Just (ih, p) -> (Just ih, p : pis)
    in  liftA2 f
              -- previous inventory and clean tag
            ( optional (liftA2 (,) (prevInvP <* eol) (patchInfoP <* eol)) <*
              -- skip offset
              replicateM_ off (skipPatchP *> eol)
            )
            -- take limit
            (atMost lim $ patchInfoP <* eol)

earlyInventoryAllP :: Parser (Either EarliestInventory MiddleInventory)
earlyInventoryAllP =
    let f Nothing           pis = Left $ EarliestInventory pis
        f (Just (prev, ti)) pis = Right $ MiddleInventory prev ti pis
    in  liftA2 f
            (optional $ liftA2 (,) (prevInvP <* eol) (tagInfoP <* eol))
            (many (patchInfoP <* eol))

{-
patchInfosOffsetP :: Int -> Parser PatchSeq
patchInfosOffsetP off = PatchSeq
    <$> pristineP
    <*> optional (eol *> prevInvP)
    <*> ( replicateM_ off (eol *> skipPatchP) *>
          many (eol *> patchInfoP)
        )
    <* eol

patchInfosLimitP :: Int -> Parser PatchSeq
patchInfosLimitP lim = PatchSeq
    <$> pristineP
    <*> optional (eol *> prevInvP)
    <*> atMost lim (eol *> patchInfoP)
-}
