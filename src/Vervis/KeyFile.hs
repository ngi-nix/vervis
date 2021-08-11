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

-- | Initial generation of key files, and later loading them.
--
-- Some programs need to generate a file, such as a signing key, and later
-- consistently use this file for program operation. And it's critical that
-- this very same file remains available. For example, if that file is an
-- encryption key used for encrypting all program state, then losing this file
-- means losing all program state.
--
-- In such a case, you may wish to have the following behavior:
--
--   * If we're in the initial program setup step, generate key files and store
--     them somewhere (file, database, etc.)
--   * If we aren't in that step anymore, require that these files are present,
--     and load them for use in the program. If a key file is missing, don't
--     just blindly generate a new one, because we *need* it to consistently be
--     the same file we originally generated. So if it's missing, report an
--     error to the user.
--   * Have a reliable way to determine whether we're in the initial setup
--     step, and make sure it's not easy to accidentally break this detection
--
-- This module, along with "Data.KeyFile", implements such a mechanism for
-- Vervis. It's really simple:
--
--   * If there are no tables in the DB, it's the initial setup phase
--   * If initial setup, require that key file doesn't exist, and generate one
--   * If not initial setup, require that key file exists
module Vervis.KeyFile
    ( isInitialSetup
    )
where

import Control.Monad.Trans.Reader (runReaderT)
import Database.Persist.Schema (SchemaBackend, hasEntities)
import Database.Persist.Schema.SQL ()
import Database.Persist.Sql (SqlBackend, ConnectionPool, runSqlPool)

-- | Check whether we're in the initial setup step, in which we create keys.
-- Otherwise, we'll only use existing keys loaded from files.
isInitialSetup :: ConnectionPool -> SchemaBackend SqlBackend -> IO Bool
isInitialSetup pool sb =
    flip runSqlPool pool . flip runReaderT (sb, "") $ not <$> hasEntities
