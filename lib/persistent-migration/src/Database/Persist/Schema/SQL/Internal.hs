{- This file is part of persistent-migration.
 -
 - Written in 2016, 2017, 2018 by fr33domlover <fr33domlover@riseup.net>.
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

-- | This is code code shared between SQL and PostgreSQL modules. Currentl it
-- exists just for the table and column quoting. If we discover that this code
-- is corrent only for PostgreSQL and other DBMSs have their own rules, then
-- the code below should probably move back to PostgreSQL module and then this
-- Internal module is not needed at all.
module Database.Persist.Schema.SQL.Internal
    ( TableName (..)
    , ColumnName (..)
    , quoteName
    , table2sql
    , column2sql
    )
where

import Data.Text (Text)

import qualified Data.Text as T

newtype TableName = TableName { unTableName :: Text }

newtype ColumnName = ColumnName { unColumnName :: Text }

quoteName :: Text -> Text
quoteName =
    let f '\0' _  = error "quoteName found \\0 character, invalid in names"
        f '"'  cs = '"' : '"' : cs
        f c    cs = c : cs
    in  T.pack . ('"' :) . T.foldr f "\""

table2sql :: TableName -> Text
table2sql = quoteName . unTableName

column2sql :: ColumnName -> Text
column2sql = quoteName . unColumnName
