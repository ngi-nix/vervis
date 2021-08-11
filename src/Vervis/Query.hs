{- This file is part of Vervis.
 -
 - Written in 2016, 2019 by fr33domlover <fr33domlover@riseup.net>.
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

-- | DB actions for long, complicated or unsafe queries. All the non-trivial
-- usage of raw SQL and so on goes into this module. Hopefully, this module
-- helps identify patterns and commonly needed but missing tools, which can
-- then be implemented and simplify the queries.
module Vervis.Query
    ( getProjectRoleAncestorWithOpQ
    )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sql.Util

import qualified Data.Text as T (intercalate)

import Database.Persist.Graph.Class
import Database.Persist.Graph.SQL
import Vervis.Model
import Vervis.Model.Role

-- | Given a project role and a project operation, find an ancestor role which
-- has access to the operation.
getProjectRoleAncestorWithOpQ
    :: MonadIO m
    => ProjectOperation
    -> RoleId
    -> ReaderT SqlBackend m (Maybe (Entity RoleAccess))
getProjectRoleAncestorWithOpQ op role = do
    conn <- ask
    let dbname = connEscapeName conn
        eAcc = entityDef $ dummyFromField RoleAccessId
        tAcc = dbname $ entityDB eAcc
        qcols =
            T.intercalate ", " $
            map ((tAcc <>) . ("." <>)) $
            entityColumnNames eAcc conn
        field :: PersistEntity record => EntityField record typ -> Text
        field = dbname . fieldDB . persistFieldDef
    listToMaybe <$>
        rawSqlWithGraph
            Ancestors
            role
            RoleInheritParent
            RoleInheritChild
            (\ temp -> mconcat
                [ "SELECT ??"
                , " FROM ", dbname temp, " INNER JOIN ", tAcc
                , " ON "
                , dbname temp, ".", field RoleInheritParent
                , " = "
                , tAcc, ".", field RoleAccessRole
                , " WHERE "
                , tAcc, ".", field RoleAccessOp
                , " = ?"
                , " LIMIT 1"
                ]
            )
            [toPersistValue op]
