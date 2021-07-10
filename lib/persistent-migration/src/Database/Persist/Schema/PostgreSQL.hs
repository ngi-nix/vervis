{- This file is part of persistent-migration.
 -
 - Written in 2016, 2017, 2018, 2019, 2020
 - by fr33domlover <fr33domlover@riseup.net>.
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

module Database.Persist.Schema.PostgreSQL
    ( schemaBackend
    )
where

import Data.Text (Text)
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Types (SqlType (..))

import qualified Data.Text as T

import Database.Persist.Schema.SQL
import Database.Persist.Schema.SQL.Internal

constraint2sql :: ConstraintName -> Text
constraint2sql = quoteName . unConstraintName

typeSql :: SqlType -> Text
typeSql SqlString               = "VARCHAR"
typeSql SqlInt32                = "INT4"
typeSql SqlInt64                = "INT8"
typeSql SqlReal                 = "DOUBLE PRECISION"
typeSql (SqlNumeric prec scale) =
    T.concat ["NUMERIC(", T.pack (show prec), ",", T.pack (show scale), ")"]
typeSql SqlDay                  = "DATE"
typeSql SqlTime                 = "TIME"
typeSql SqlDayTime              = "TIMESTAMP WITH TIME ZONE"
typeSql SqlBlob                 = "BYTEA"
typeSql SqlBool                 = "BOOLEAN"
typeSql (SqlOther t)            = t

columnSql :: Column -> Text
columnSql (Column name typ mnull) = mconcat
    [ column2sql name, " "
    , typeSql typ
    , case mnull of
        MaybeNull -> " NULL"
        NotNull   -> " NOT NULL"
    ]

idCol :: ColumnName
idCol = ColumnName "id"

idSql :: Text
idSql = "id SERIAL8 PRIMARY KEY UNIQUE"

schemaBackend :: SchemaBackend SqlBackend
schemaBackend = SqlSchemaBackend
    { ssbRefType = SqlInt64
    , ssbDoesTableExist =
        "SELECT COUNT(*) FROM pg_catalog.pg_tables \
        \ WHERE schemaname != 'pg_catalog' AND \
              \ schemaname != 'information_schema' AND \
              \ tablename = ?"
    , ssbAnyTablesExist =
        "SELECT EXISTS (SELECT 1 FROM pg_catalog.pg_tables \
            \ WHERE schemaname != 'pg_catalog' AND \
                  \ schemaname != 'information_schema')"
    , ssbGetTableNames =
        "SELECT table_name FROM information_schema.tables \
        \ WHERE table_catalog = current_database() AND \
              \ table_schema  = current_schema() AND \
              \ table_name   != ?"
    , ssbGetTableColumnNames =
        "SELECT column_name FROM information_schema.columns \
        \ WHERE table_catalog = current_database() AND \
              \ table_schema  = current_schema() AND \
              \ table_name    = ?"
    , ssbAnyRowsExist = \ table -> mconcat
        [ "SELECT EXISTS (SELECT 1 FROM ", table2sql table, ")"
        ]
    , ssbCreateTable = \ table columns -> mconcat
        [ "CREATE TABLE ", table2sql table, " ("
        , idSql
        , if null columns then T.empty else ", "
        , T.intercalate ", " $ map columnSql columns
        , ")"
        ]
    , ssbRenameTable = \ old new -> mconcat
        [ "ALTER TABLE ", table2sql old
        , " RENAME TO ", table2sql new
        ]
    , ssbDropTable = \ table -> mconcat
        [ "DROP TABLE ", table2sql table
        ]
    , ssbAddColumn = \ table column withdef -> mconcat
        [ "ALTER TABLE ", table2sql table
        , " ADD COLUMN ", columnSql column
        , if withdef
            then " DEFAULT ?"
            else T.empty
        ]
    , ssbRenameColumn = \ table old new -> mconcat
        [ "ALTER TABLE ", table2sql table
        , " RENAME COLUMN ", column2sql old, " TO ", column2sql new
        ]
    , ssbRetypeColumn = \ table column typ -> mconcat
        [ "ALTER TABLE ", table2sql table
        , " ALTER COLUMN ", column2sql column
        , " TYPE ", typeSql typ
        ]
    , ssbRetypeColumnConst = \ table column typ -> mconcat
        [ "ALTER TABLE ", table2sql table
        , " ALTER COLUMN ", column2sql column
        , " TYPE ", typeSql typ
        , " USING ?"
        ]
    , ssbRenullColumn = \ table column mnull -> mconcat
        [ "ALTER TABLE ", table2sql table
        , " ALTER COLUMN ", column2sql column
        , case mnull of
            MaybeNull -> " DROP"
            NotNull   -> " SET"
        , " NOT NULL"
        ]
    , ssbUnnullColumn = \ table column -> mconcat
        [ "UPDATE ", table2sql table
        , " SET ", column2sql column, " = ?"
        , " WHERE ", column2sql column, " IS NULL"
        ]
    , ssbDefColumn = \ table column -> mconcat
        [ "ALTER TABLE ", table2sql table
        , " ALTER COLUMN ", column2sql column
        , " SET DEFAULT ?"
        ]
    , ssbUndefColumn = \ table column -> mconcat
        [ "ALTER TABLE ", table2sql table
        , " ALTER COLUMN ", column2sql column
        , " DROP DEFAULT"
        ]
    , ssbDropColumn = \ table column -> mconcat
        [ "ALTER TABLE ", table2sql table
        , " DROP COLUMN ", column2sql column
        ]
    , ssbAddUnique = \ table constraint columns -> mconcat
        [ "ALTER TABLE ", table2sql table
        , " ADD CONSTRAINT ", constraint2sql constraint
        , " UNIQUE("
        , T.intercalate ", " $ map column2sql columns
        , ")"
        ]
    , ssbAddForeignKey = \ table constraint column target -> mconcat
        [ "ALTER TABLE ", table2sql table
        , " ADD CONSTRAINT ", constraint2sql constraint
        , " FOREIGN KEY(", column2sql column
        , ") REFERENCES ", table2sql target, "(", column2sql idCol, ")"
        ]
    , ssbRenameConstraint = \ _table old new -> mconcat
        [ "ALTER INDEX ", constraint2sql old
        , " RENAME TO ", constraint2sql new
        ]
    , ssbDropConstraint = \ table constraint -> mconcat
        [ "ALTER TABLE ", table2sql table
        , " DROP CONSTRAINT ", constraint2sql constraint
        ]
    }
