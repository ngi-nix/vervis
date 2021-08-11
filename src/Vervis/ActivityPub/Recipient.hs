{- This file is part of Vervis.
 -
 - Written in 2019, 2020 by fr33domlover <fr33domlover@riseup.net>.
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

module Vervis.ActivityPub.Recipient
    ( LocalActor (..)
    , LocalPersonCollection (..)
    , LocalTicketDirectSet (..)
    , LocalPatchDirectSet (..)
    , LocalProjectDirectSet (..)
    , LocalProjectRelatedSet (..)
    , LocalRepoDirectSet (..)
    , LocalRepoRelatedSet (..)
    , LocalSharerDirectSet (..)
    , LocalSharerRelatedSet (..)
    , LocalRecipientSet
    , concatRecipients
    , parseLocalActor
    , renderLocalActor
    , renderLocalPersonCollection
    , makeRecipientSet
    , ParsedAudience (..)
    , parseAudience
    , actorRecips
    , localRecipSieve
    , localRecipSieve'

    , Aud (..)
    , collectAudience
    )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Either
import Data.Foldable
import Data.List ((\\))
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Maybe
import Data.Semigroup
import Data.Text (Text)
import Data.These
import Data.Traversable

import qualified Data.List.NonEmpty as NE
import qualified Data.List.Ordered as LO
import qualified Data.Text as T

import Network.FedURI
import Web.ActivityPub hiding (Ticket)
import Yesod.ActivityPub
import Yesod.FedURI
import Yesod.Hashids
import Yesod.MonadSite

import Data.List.Local
import Data.List.NonEmpty.Local

import Vervis.FedURI
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident

concatRecipients :: Audience u -> [ObjURI u]
concatRecipients (Audience to bto cc bcc gen _) =
    concat [to, bto, cc, bcc, gen]

-------------------------------------------------------------------------------
-- Actor and collection-of-persons types
--
-- These are the 2 kinds of local recipients. This is the starting point for
-- grouping and checking recipient lists: First parse recipient URIs into these
-- types, then you can do any further parsing and grouping.
-------------------------------------------------------------------------------

data LocalActor
    = LocalActorSharer ShrIdent
    | LocalActorProject ShrIdent PrjIdent
    | LocalActorRepo ShrIdent RpIdent
    deriving (Eq, Ord)

parseLocalActor :: Route App -> Maybe LocalActor
parseLocalActor (SharerR shr)      = Just $ LocalActorSharer shr
parseLocalActor (ProjectR shr prj) = Just $ LocalActorProject shr prj
parseLocalActor (RepoR shr rp)     = Just $ LocalActorRepo shr rp
parseLocalActor _                  = Nothing

renderLocalActor :: LocalActor -> Route App
renderLocalActor (LocalActorSharer shr)      = SharerR shr
renderLocalActor (LocalActorProject shr prj) = ProjectR shr prj
renderLocalActor (LocalActorRepo shr rp)     = RepoR shr rp

data LocalPersonCollection
    = LocalPersonCollectionSharerFollowers       ShrIdent
    | LocalPersonCollectionSharerTicketTeam      ShrIdent (KeyHashid TicketAuthorLocal)
    | LocalPersonCollectionSharerTicketFollowers ShrIdent (KeyHashid TicketAuthorLocal)
    | LocalPersonCollectionSharerPatchFollowers  ShrIdent (KeyHashid TicketAuthorLocal)

    | LocalPersonCollectionProjectTeam             ShrIdent PrjIdent
    | LocalPersonCollectionProjectFollowers        ShrIdent PrjIdent
    | LocalPersonCollectionProjectTicketTeam       ShrIdent PrjIdent (KeyHashid LocalTicket)
    | LocalPersonCollectionProjectTicketFollowers  ShrIdent PrjIdent (KeyHashid LocalTicket)

    | LocalPersonCollectionRepoTeam           ShrIdent RpIdent
    | LocalPersonCollectionRepoFollowers      ShrIdent RpIdent
    | LocalPersonCollectionRepoPatchFollowers ShrIdent RpIdent (KeyHashid LocalTicket)
    deriving (Eq, Ord)

parseLocalPersonCollection
    :: Route App -> Maybe LocalPersonCollection
parseLocalPersonCollection (SharerFollowersR shr) =
    Just $ LocalPersonCollectionSharerFollowers shr
parseLocalPersonCollection (SharerTicketTeamR shr talkhid) =
    Just $ LocalPersonCollectionSharerTicketTeam shr talkhid
parseLocalPersonCollection (SharerTicketFollowersR shr talkhid) =
    Just $ LocalPersonCollectionSharerTicketFollowers shr talkhid
parseLocalPersonCollection (SharerPatchFollowersR shr talkhid) =
    Just $ LocalPersonCollectionSharerPatchFollowers shr talkhid
parseLocalPersonCollection (ProjectTeamR shr prj) =
    Just $ LocalPersonCollectionProjectTeam shr prj
parseLocalPersonCollection (ProjectFollowersR shr prj) =
    Just $ LocalPersonCollectionProjectFollowers shr prj
parseLocalPersonCollection (ProjectTicketTeamR shr prj num) =
    Just $ LocalPersonCollectionProjectTicketTeam shr prj num
parseLocalPersonCollection (ProjectTicketParticipantsR shr prj num) =
    Just $ LocalPersonCollectionProjectTicketFollowers shr prj num
parseLocalPersonCollection (RepoTeamR shr rp) =
    Just $ LocalPersonCollectionRepoTeam shr rp
parseLocalPersonCollection (RepoFollowersR shr rp) =
    Just $ LocalPersonCollectionRepoFollowers shr rp
parseLocalPersonCollection (RepoPatchFollowersR shr rp ltkhid) =
    Just $ LocalPersonCollectionRepoPatchFollowers shr rp ltkhid
parseLocalPersonCollection _ = Nothing

renderLocalPersonCollection :: LocalPersonCollection -> Route App
renderLocalPersonCollection (LocalPersonCollectionSharerFollowers shr)                   = SharerFollowersR shr
renderLocalPersonCollection (LocalPersonCollectionSharerTicketTeam shr talkhid)          = SharerTicketTeamR shr talkhid
renderLocalPersonCollection (LocalPersonCollectionSharerTicketFollowers shr talkhid)     = SharerTicketFollowersR shr talkhid
renderLocalPersonCollection (LocalPersonCollectionSharerPatchFollowers shr talkhid)      = SharerPatchFollowersR shr talkhid
renderLocalPersonCollection (LocalPersonCollectionProjectTeam shr prj)                   = ProjectTeamR shr prj
renderLocalPersonCollection (LocalPersonCollectionProjectFollowers shr prj)              = ProjectFollowersR shr prj
renderLocalPersonCollection (LocalPersonCollectionProjectTicketTeam shr prj ltkhid)      = ProjectTicketTeamR shr prj ltkhid
renderLocalPersonCollection (LocalPersonCollectionProjectTicketFollowers shr prj ltkhid) = ProjectTicketParticipantsR shr prj ltkhid
renderLocalPersonCollection (LocalPersonCollectionRepoTeam shr rp)                       = RepoTeamR shr rp
renderLocalPersonCollection (LocalPersonCollectionRepoFollowers shr rp)                  = RepoFollowersR shr rp
renderLocalPersonCollection (LocalPersonCollectionRepoPatchFollowers shr rp ltkhid)      = RepoPatchFollowersR shr rp ltkhid

parseLocalRecipient
    :: Route App -> Maybe (Either LocalActor LocalPersonCollection)
parseLocalRecipient r =
    Left <$> parseLocalActor r <|> Right <$> parseLocalPersonCollection r

-------------------------------------------------------------------------------
-- Intermediate recipient types
--
-- These are here just to help with grouping recipients. From this
-- representation it's easy to group recipients into a form that is friendly to
-- the code that fetches the actual recipients from the DB.
-------------------------------------------------------------------------------

data LocalTicketRecipientDirect = LocalTicketTeam | LocalTicketFollowerz
    deriving (Eq, Ord)

data LocalPatchRecipientDirect = LocalPatchFollowers deriving (Eq, Ord)

data LocalProjectRecipientDirect
    = LocalProject
    | LocalProjectTeam
    | LocalProjectFollowers
    deriving (Eq, Ord)

data LocalProjectRecipient
    = LocalProjectDirect LocalProjectRecipientDirect
    | LocalProjectTicketRelated (KeyHashid LocalTicket) LocalTicketRecipientDirect
    deriving (Eq, Ord)

data LocalRepoRecipientDirect
    = LocalRepo
    | LocalRepoTeam
    | LocalRepoFollowers
    deriving (Eq, Ord)

data LocalRepoRecipient
    = LocalRepoDirect LocalRepoRecipientDirect
    | LocalRepoPatchRelated (KeyHashid LocalTicket) LocalPatchRecipientDirect
    deriving (Eq, Ord)

data LocalSharerRecipientDirect
    = LocalSharer
    | LocalSharerFollowers
    deriving (Eq, Ord)

data LocalSharerRecipient
    = LocalSharerDirect LocalSharerRecipientDirect
    | LocalSharerTicketRelated (KeyHashid TicketAuthorLocal) LocalTicketRecipientDirect
    | LocalSharerPatchRelated (KeyHashid TicketAuthorLocal) LocalPatchRecipientDirect
    | LocalProjectRelated PrjIdent LocalProjectRecipient
    | LocalRepoRelated RpIdent LocalRepoRecipient
    deriving (Eq, Ord)

data LocalGroupedRecipient = LocalSharerRelated ShrIdent LocalSharerRecipient
    deriving (Eq, Ord)

groupedRecipientFromActor :: LocalActor -> LocalGroupedRecipient
groupedRecipientFromActor (LocalActorSharer shr) =
    LocalSharerRelated shr $ LocalSharerDirect LocalSharer
groupedRecipientFromActor (LocalActorProject shr prj) =
    LocalSharerRelated shr $ LocalProjectRelated prj $
        LocalProjectDirect LocalProject
groupedRecipientFromActor (LocalActorRepo shr rp) =
    LocalSharerRelated shr $ LocalRepoRelated rp $ LocalRepoDirect LocalRepo

groupedRecipientFromCollection
    :: LocalPersonCollection -> LocalGroupedRecipient
groupedRecipientFromCollection
    (LocalPersonCollectionSharerFollowers shr) =
        LocalSharerRelated shr $ LocalSharerDirect LocalSharerFollowers
groupedRecipientFromCollection
    (LocalPersonCollectionSharerTicketTeam shr talkhid) =
        LocalSharerRelated shr $
            LocalSharerTicketRelated talkhid LocalTicketTeam
groupedRecipientFromCollection
    (LocalPersonCollectionSharerTicketFollowers shr talkhid) =
        LocalSharerRelated shr $
            LocalSharerTicketRelated talkhid LocalTicketFollowerz
groupedRecipientFromCollection
    (LocalPersonCollectionSharerPatchFollowers shr talkhid) =
        LocalSharerRelated shr $
            LocalSharerPatchRelated talkhid LocalPatchFollowers
groupedRecipientFromCollection
    (LocalPersonCollectionProjectTeam shr prj) =
        LocalSharerRelated shr $ LocalProjectRelated prj $
            LocalProjectDirect LocalProjectTeam
groupedRecipientFromCollection
    (LocalPersonCollectionProjectFollowers shr prj) =
        LocalSharerRelated shr $ LocalProjectRelated prj $
            LocalProjectDirect LocalProjectFollowers
groupedRecipientFromCollection
    (LocalPersonCollectionProjectTicketTeam shr prj num) =
        LocalSharerRelated shr $ LocalProjectRelated prj $
            LocalProjectTicketRelated num LocalTicketTeam
groupedRecipientFromCollection
    (LocalPersonCollectionProjectTicketFollowers shr prj num) =
        LocalSharerRelated shr $ LocalProjectRelated prj $
            LocalProjectTicketRelated num LocalTicketFollowerz
groupedRecipientFromCollection
    (LocalPersonCollectionRepoTeam shr rp) =
        LocalSharerRelated shr $ LocalRepoRelated rp $
            LocalRepoDirect LocalRepoTeam
groupedRecipientFromCollection
    (LocalPersonCollectionRepoFollowers shr rp) =
        LocalSharerRelated shr $ LocalRepoRelated rp $
            LocalRepoDirect LocalRepoFollowers
groupedRecipientFromCollection
    (LocalPersonCollectionRepoPatchFollowers shr rp ltkhid) =
        LocalSharerRelated shr $ LocalRepoRelated rp $
            LocalRepoPatchRelated ltkhid LocalPatchFollowers

-------------------------------------------------------------------------------
-- Recipient set types
--
-- These types represent a set of recipients grouped by the variable components
-- of their routes. It's convenient to use when looking for the recipients in
-- the DB, and easy to manipulate and check the recipient list in terms of app
-- logic rather than plain lists of routes.
-------------------------------------------------------------------------------

data LocalTicketDirectSet = LocalTicketDirectSet
    { localRecipTicketTeam      :: Bool
    , localRecipTicketFollowers :: Bool
    }
    deriving Eq

newtype LocalPatchDirectSet = LocalPatchDirectSet
    { localRecipPatchFollowers :: Bool
    }
    deriving Eq

data LocalProjectDirectSet = LocalProjectDirectSet
    { localRecipProject          :: Bool
    , localRecipProjectTeam      :: Bool
    , localRecipProjectFollowers :: Bool
    }
    deriving Eq

data LocalProjectRelatedSet = LocalProjectRelatedSet
    { localRecipProjectDirect
        :: LocalProjectDirectSet
    , localRecipProjectTicketRelated
        :: [(KeyHashid LocalTicket, LocalTicketDirectSet)]
    }
    deriving Eq

data LocalRepoDirectSet = LocalRepoDirectSet
    { localRecipRepo          :: Bool
    , localRecipRepoTeam      :: Bool
    , localRecipRepoFollowers :: Bool
    }
    deriving Eq

data LocalRepoRelatedSet = LocalRepoRelatedSet
    { localRecipRepoDirect
        :: LocalRepoDirectSet
    , localRecipRepoPatchRelated
        :: [(KeyHashid LocalTicket, LocalPatchDirectSet)]
    }
    deriving Eq

data LocalSharerDirectSet = LocalSharerDirectSet
    { localRecipSharer          :: Bool
    , localRecipSharerFollowers :: Bool
    }
    deriving Eq

data LocalSharerRelatedSet = LocalSharerRelatedSet
    { localRecipSharerDirect
        :: LocalSharerDirectSet
    , localRecipSharerTicketRelated
        :: [(KeyHashid TicketAuthorLocal, LocalTicketDirectSet)]
    , localRecipSharerPatchRelated
        :: [(KeyHashid TicketAuthorLocal, LocalPatchDirectSet)]
    , localRecipProjectRelated
        :: [(PrjIdent, LocalProjectRelatedSet)]
    , localRecipRepoRelated
        :: [(RpIdent, LocalRepoRelatedSet)]
    }
    deriving Eq

type LocalRecipientSet = [(ShrIdent, LocalSharerRelatedSet)]

groupLocalRecipients :: [LocalGroupedRecipient] -> LocalRecipientSet
groupLocalRecipients
    = map (second lsr2set)
    . groupAllExtract
        (\ (LocalSharerRelated shr _) -> shr)
        (\ (LocalSharerRelated _ lsr) -> lsr)
    where
    lsr2set = mk . partitionLSR . NE.toList
        where
        partitionLSR = foldr f ([], [], [], [], [])
            where
            f i (ds, ts, ps, js, rs) =
                case i of
                    LocalSharerDirect d ->
                        (d:ds, ts, ps, js, rs)
                    LocalSharerTicketRelated talkhid ltr ->
                        (ds, (talkhid, ltr):ts, ps, js, rs)
                    LocalSharerPatchRelated talkhid lpr ->
                        (ds, ts, (talkhid, lpr):ps, js, rs)
                    LocalProjectRelated prj ljr ->
                        (ds, ts, ps, (prj, ljr):js, rs)
                    LocalRepoRelated rp lrr ->
                        (ds, ts, ps, js, (rp, lrr):rs)
        mk (ds, ts, ps, js, rs) =
            LocalSharerRelatedSet
                (lsrs2set ds)
                (map (second ltrs2set) $ groupWithExtract fst snd ts)
                (map (second lprs2set) $ groupWithExtract fst snd ps)
                (map (second ljr2set) $ groupWithExtract fst snd js)
                (map (second lrr2set) $ groupWithExtract fst snd rs)
            where
            lsrs2set = foldl' f initial
                where
                initial = LocalSharerDirectSet False False
                f s LocalSharer =
                    s { localRecipSharer = True }
                f s LocalSharerFollowers =
                    s { localRecipSharerFollowers = True }
            ltrs2set = foldl' f initial
                where
                initial = LocalTicketDirectSet False False
                f s LocalTicketTeam =
                    s { localRecipTicketTeam = True }
                f s LocalTicketFollowerz =
                    s { localRecipTicketFollowers = True }
            lprs2set = foldl' f initial
                where
                initial = LocalPatchDirectSet False
                f s LocalPatchFollowers = s { localRecipPatchFollowers = True }
            ljr2set = uncurry mk . partitionEithers . map ljr2e . NE.toList
                where
                ljr2e (LocalProjectDirect d)               = Left d
                ljr2e (LocalProjectTicketRelated num ltrs) = Right (num, ltrs)
                mk ds ts =
                    LocalProjectRelatedSet
                        (ljrs2set ds)
                        (map (second ltrs2set) $ groupWithExtract fst snd ts)
                    where
                    ljrs2set = foldl' f initial
                        where
                        initial = LocalProjectDirectSet False False False
                        f s LocalProject =
                            s { localRecipProject = True }
                        f s LocalProjectTeam =
                            s { localRecipProjectTeam = True }
                        f s LocalProjectFollowers =
                            s { localRecipProjectFollowers = True }
            lrr2set = uncurry mk . partitionEithers . map lrr2e . NE.toList
                where
                lrr2e (LocalRepoDirect d)              = Left d
                lrr2e (LocalRepoPatchRelated num ltrs) = Right (num, ltrs)
                mk ds ps =
                    LocalRepoRelatedSet
                        (lrrs2set ds)
                        (map (second lprs2set) $ groupWithExtract fst snd ps)
                    where
                    lrrs2set = foldl' f initial
                        where
                        initial = LocalRepoDirectSet False False False
                        f s LocalRepo          = s { localRecipRepo = True }
                        f s LocalRepoTeam      = s { localRecipRepoTeam = True }
                        f s LocalRepoFollowers = s { localRecipRepoFollowers = True }

-------------------------------------------------------------------------------
-- Parse URIs into a grouped recipient set
-------------------------------------------------------------------------------

makeRecipientSet
    :: [LocalActor] -> [LocalPersonCollection] -> LocalRecipientSet
makeRecipientSet actors collections =
    groupLocalRecipients $
        map groupedRecipientFromActor actors ++
        map groupedRecipientFromCollection collections

parseRecipients
    :: (MonadSite m, SiteEnv m ~ App)
    => NonEmpty FedURI
    -> ExceptT Text m (LocalRecipientSet, [FedURI])
parseRecipients recips = do
    hLocal <- asksSite siteInstanceHost
    let (locals, remotes) = splitRecipients hLocal recips
        (lusInvalid, routesInvalid, localsSet) = parseLocalRecipients locals
    unless (null lusInvalid) $
        throwE $
            "Local recipients are invalid routes: " <>
            T.pack (show $ map (renderObjURI . ObjURI hLocal) lusInvalid)
    unless (null routesInvalid) $ do
        renderUrl <- askUrlRender
        throwE $
            "Local recipients are non-recipient routes: " <>
            T.pack (show $ map renderUrl routesInvalid)
    return (localsSet, remotes)
    where
    splitRecipients :: Host -> NonEmpty FedURI -> ([LocalURI], [FedURI])
    splitRecipients home recips =
        let (local, remote) = NE.partition ((== home) . objUriAuthority) recips
        in  (map objUriLocal local, remote)

    parseLocalRecipients
        :: [LocalURI] -> ([LocalURI], [Route App], LocalRecipientSet)
    parseLocalRecipients lus =
        let (lusInvalid, routes) = partitionEithers $ map parseRoute lus
            (routesInvalid, recips) = partitionEithers $ map parseRecip routes
            (actors, collections) = partitionEithers recips
            grouped =
                map groupedRecipientFromActor actors ++
                map groupedRecipientFromCollection collections
        in  (lusInvalid, routesInvalid, groupLocalRecipients grouped)
        where
        parseRoute lu =
            case decodeRouteLocal lu of
                Nothing -> Left lu
                Just route -> Right route
        parseRecip route =
            case parseLocalRecipient route of
                Nothing -> Left route
                Just recip -> Right recip

data ParsedAudience u = ParsedAudience
    { paudLocalRecips  :: LocalRecipientSet
    , paudRemoteActors :: [(Authority u, NonEmpty LocalURI)]
    , paudBlinded      :: Audience u
    , paudFwdHosts     :: [Authority u]
    }

parseAudience
    :: (MonadSite m, SiteEnv m ~ App)
    => Audience URIMode
    -> ExceptT Text m (Maybe (ParsedAudience URIMode))
parseAudience audience = do
    let recips = concatRecipients audience
    for (nonEmpty recips) $ \ recipsNE -> do
        (localsSet, remotes) <- parseRecipients recipsNE
        let remotesGrouped =
                groupByHost $ remotes \\ audienceNonActors audience
            hosts = map fst remotesGrouped
        return ParsedAudience
            { paudLocalRecips  = localsSet
            , paudRemoteActors = remotesGrouped
            , paudBlinded      =
                audience { audienceBto = [], audienceBcc = [] }
            , paudFwdHosts     =
                let nonActorHosts =
                        LO.nubSort $
                            map objUriAuthority $ audienceNonActors audience
                in  LO.isect hosts nonActorHosts
            }
    where
    groupByHost :: [FedURI] -> [(Host, NonEmpty LocalURI)]
    groupByHost = groupAllExtract objUriAuthority objUriLocal

actorIsMember :: LocalActor -> LocalRecipientSet -> Bool
actorIsMember (LocalActorSharer shr) lrSet =
    case lookup shr lrSet of
        Just lsrSet -> localRecipSharer $ localRecipSharerDirect lsrSet
        Nothing -> False
actorIsMember (LocalActorProject shr prj) lrSet = fromMaybe False $ do
    lsrSet <- lookup shr lrSet
    lprSet <- lookup prj $ localRecipProjectRelated lsrSet
    return $ localRecipProject $ localRecipProjectDirect $ lprSet
actorIsMember (LocalActorRepo shr rp) lrSet = fromMaybe False $ do
    lsrSet <- lookup shr lrSet
    lrrSet <- lookup rp $ localRecipRepoRelated lsrSet
    return $ localRecipRepo $ localRecipRepoDirect $ lrrSet

actorRecips :: LocalActor -> LocalRecipientSet
actorRecips = groupLocalRecipients . (: []) . groupedRecipientFromActor

localRecipSieve
    :: LocalRecipientSet -> Bool -> LocalRecipientSet -> LocalRecipientSet
localRecipSieve sieve allowActors =
    localRecipSieve' sieve allowActors allowActors

localRecipSieve'
    :: LocalRecipientSet
    -> Bool
    -> Bool
    -> LocalRecipientSet
    -> LocalRecipientSet
localRecipSieve' sieve allowSharers allowOthers =
    mapMaybe (uncurry applySharerRelated) . sortAlign sieve
    where
    onlyActorsJ (LocalProjectRelatedSet (LocalProjectDirectSet j _t _f) _ts) =
        LocalProjectRelatedSet (LocalProjectDirectSet (j && allowOthers) False False) []
    onlyActorsR (LocalRepoRelatedSet (LocalRepoDirectSet r _t _f) _ps) =
        LocalRepoRelatedSet (LocalRepoDirectSet (r && allowOthers) False False) []
    onlyActorsS (LocalSharerRelatedSet (LocalSharerDirectSet s _f) _ts _ps js rs) =
        LocalSharerRelatedSet
            (LocalSharerDirectSet (s && allowSharers) False)
            []
            []
            (map (second onlyActorsJ) js)
            (map (second onlyActorsR) rs)

    applySharerRelated _   (This _) = Nothing
    applySharerRelated shr (That s) =
        if allowSharers || allowOthers
            then Just (shr, onlyActorsS s)
            else Nothing
    applySharerRelated shr (These (LocalSharerRelatedSet s' t' p' j' r') (LocalSharerRelatedSet s t p j r)) =
        Just
            ( shr
            , LocalSharerRelatedSet
                (applySharer s' s)
                (mapMaybe (uncurry applyTicketRelated) $ sortAlign t' t)
                (mapMaybe (uncurry applyPatchRelated) $ sortAlign p' p)
                (mapMaybe (uncurry applyProjectRelated) $ sortAlign j' j)
                (mapMaybe (uncurry applyRepoRelated) $ sortAlign r' r)
            )
        where
        applySharer (LocalSharerDirectSet s' f') (LocalSharerDirectSet s f) =
            LocalSharerDirectSet (s && (s' || allowSharers)) (f && f')

        applyTicketRelated ltkhid (These t' t) = Just (ltkhid, applyTicket t' t)
            where
            applyTicket (LocalTicketDirectSet t' f') (LocalTicketDirectSet t f) =
                LocalTicketDirectSet (t && t') (f && f')
        applyTicketRelated _      _            = Nothing

        applyPatchRelated ltkhid (These p' p) = Just (ltkhid, applyPatch p' p)
            where
            applyPatch (LocalPatchDirectSet f') (LocalPatchDirectSet f) =
                LocalPatchDirectSet $ f && f'
        applyPatchRelated _      _            = Nothing

        applyProjectRelated _   (This _) = Nothing
        applyProjectRelated prj (That j) =
            if allowOthers
                then Just (prj, onlyActorsJ j)
                else Nothing
        applyProjectRelated prj (These (LocalProjectRelatedSet j' t') (LocalProjectRelatedSet j t)) =
            Just
                ( prj
                , LocalProjectRelatedSet
                    (applyProject j' j)
                    (mapMaybe (uncurry applyTicketRelated) $ sortAlign t' t)
                )
            where
            applyProject (LocalProjectDirectSet j' t' f') (LocalProjectDirectSet j t f) =
                LocalProjectDirectSet (j && (j' || allowOthers)) (t && t') (f && f')

        applyRepoRelated _  (This _) = Nothing
        applyRepoRelated rp (That r) =
            if allowOthers
                then Just (rp, onlyActorsR r)
                else Nothing
        applyRepoRelated rp (These (LocalRepoRelatedSet r' p') (LocalRepoRelatedSet r p)) =
            Just
                ( rp
                , LocalRepoRelatedSet
                    (applyRepo r' r)
                    (mapMaybe (uncurry applyPatchRelated) $ sortAlign p' p)
                )
            where
            applyRepo (LocalRepoDirectSet r' t' f') (LocalRepoDirectSet r t f) =
                LocalRepoDirectSet (r && (r' || allowOthers)) (t && t') (f && f')

data Aud u
    = AudLocal [LocalActor] [LocalPersonCollection]
    | AudRemote (Authority u) [LocalURI] [LocalURI]

collectAudience
    :: Foldable f
    => f (Aud u)
    -> ( LocalRecipientSet
       , [(Authority u, NonEmpty LocalURI)]
       , [Authority u]
       , [Route App]
       , [ObjURI u]
       )
collectAudience auds =
    let (locals, remotes) = partitionAudience auds
        (actors, collections) =
            let organize = LO.nubSort . concat
            in  bimap organize organize $ unzip locals
        groupedRemotes =
            let organize = LO.nubSort . sconcat
            in  map (second $ bimap organize organize . NE.unzip) $
                groupAllExtract fst snd remotes
    in  ( makeRecipientSet actors collections
        , mapMaybe (\ (h, (as, _)) -> (h,) <$> nonEmpty as) groupedRemotes
        , [ h | (h, (_, cs)) <- groupedRemotes, not (null cs) ]
        , map renderLocalActor actors ++
          map renderLocalPersonCollection collections
        , concatMap (\ (h, (as, cs)) -> ObjURI h <$> as ++ cs) groupedRemotes
        )
    where
    partitionAudience = foldl' f ([], [])
        where
        f (ls, rs) (AudLocal as cs)    = ((as, cs) : ls, rs)
        f (ls, rs) (AudRemote h as cs) = (ls           , (h, (as, cs)) : rs)
