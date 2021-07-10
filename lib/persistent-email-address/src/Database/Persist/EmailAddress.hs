{- This file is part of persistent-email-address.
 -
 - Written in 2018 by fr33domlover <fr33domlover@riseup.net>.
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

-- | Orphan instances for 'EmailAddress', allowing it to be used as a
-- @persistent@ field. Note that the address isn't validated when parsed,
-- relying on it already being validated before insertion into the database. If
-- you do want it to be parsed every single time, you can use the instances
-- from the @emailaddress@ package.
module Database.Persist.EmailAddress
    (
    )
where

import Control.Monad ((<=<))
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Database.Persist.Class (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..))
import Database.Persist.Types (SqlType (..))
import Text.Email.Validate (EmailAddress, toByteString, unsafeEmailAddress)

import qualified Data.ByteString.Char8 as B (uncons, break)

instance PersistField EmailAddress where
    toPersistValue =
        toPersistValue . decodeUtf8With lenientDecode . toByteString
    fromPersistValue =
        let fromBS b =
                let (l, ad) = B.break (== '@') b
                in  case B.uncons ad of
                        Nothing ->
                            Left $ pack "No at-sign found in email address"
                        Just (_, d) ->
                            Right $ unsafeEmailAddress l d
        in  fromBS <=< fmap encodeUtf8 . fromPersistValue

instance PersistFieldSql EmailAddress where
    sqlType _ = SqlString

