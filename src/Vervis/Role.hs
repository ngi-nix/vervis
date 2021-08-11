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

module Vervis.Role
    ( getProjectRoleGraph
    )
where

import Control.Arrow (second, (&&&), (***))
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Tuple (swap)
import Database.Esqueleto
import Yesod.Persist.Core (runDB)

import qualified Data.HashMap.Lazy as M (fromList, lookup)
import qualified Database.Persist as P

import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident

getProjectRoleGraph :: SharerId -> AppDB (Gr RlIdent ())
getProjectRoleGraph sid = do
    (roles, inhs) <- do
        prs <- P.selectList [RoleSharer P.==. sid] []
        prhs <- select $ from $ \ (pr `InnerJoin` prh) -> do
            on $ pr ^. RoleId ==. prh ^. RoleInheritParent
            where_ $ pr ^. RoleSharer ==. val sid
            return prh
        return (prs, prhs)
    let numbered = zip [1..] roles
        nodes = map (second $ roleIdent . entityVal) numbered
        nodeMap = M.fromList $ map (swap . second entityKey) numbered
        pridToNode prid =
            case M.lookup prid nodeMap of
                Nothing -> error "Role graph: Node not found in node map"
                Just n  -> n
        edges =
            map
                ( (\ (c, p) -> (c, p, ()))
                . (pridToNode *** pridToNode)
                . (roleInheritChild &&& roleInheritParent)
                . entityVal
                )
                inhs
    return $ mkGraph nodes edges
