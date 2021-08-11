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

module Data.Paginate.Local
    ( -- * Settings
      -- ** Jump settings
      JumpSettings ()
    , jumpMin
    , jumpFactor
    , jumpRound
      -- ** Navigation settings
    , NavSettings ()
    , navEdges
    , navJump
    , navNext
      -- ** Pagination settings
    , PaginateSettings ()
    , psSelect
    , psCurrent
    , psPer
      -- * Results
      -- ** Navigation controls
    , NavModel ()
    , nmFirst
    , nmPrevJumps
    , nmPrev
    , nmCurrent
    , nmTotal
    , nmNext
    , nmNextJumps
    , nmLast
      -- ** Paginate
    , paginateMaybe
    , paginateCount
    , paginateTop
    )
where

import Data.Default.Class
import Data.Maybe
import Data.Ratio

data JumpSettings = JumpSettings
    { -- | Minimal jump size to display. Smaller jumps will be discarded.
      jumpMin    :: Int
      -- | Ratio of size of consecutive jumps.
    , jumpFactor :: Ratio Int
      -- | Round jump page numbers to be multiples of this number. 1 means no
      -- rounding. 10 means all jumps will be to page numbers that are
      -- multiples of 10. And so on.
    , jumpRound  :: Int
    }

instance Default JumpSettings where
    def = JumpSettings
        { jumpMin    = 10
        , jumpFactor = 2 % 3
        , jumpRound  = 10
        }

data NavSettings = NavSettings
    { -- | Whether to always show links to first and last pages
      navEdges :: Bool
      -- | Whether and how to show jump links
    , navJump  :: Maybe JumpSettings
      -- | Number of next\/prev page links to show on each side of the current
      -- page.
    , navNext  :: Int
    }

instance Default NavSettings where
    def = NavSettings
        { navEdges = True
        , navJump  = Just def
        , navNext  = 3
        }

data PaginateSettings m f i = PaginateSettings
    { -- | Get the total number of items being paginated, and given an offset
      -- and a limit, get the specified subset of the items. The offset tells
      -- you how many items to skip from the beginning of the list, and then
      -- the limit says how many items you should take after skipping.
      psSelect  :: Int -> Int -> m (Int, f i)
      -- | Get the current page
    , psCurrent :: m (Maybe Int)
      -- | How many items to list in one page
    , psPer     :: Int
    }

instance Default (PaginateSettings m f i) where
    def = PaginateSettings
        { psSelect  = error "You didn't implement psSelect"
        , psCurrent = error "You didn't implement psCurrent"
        , psPer     = 30
        }

data NavModel = NavModel
    {
      nmFirst     :: Bool
    , nmPrevJumps :: [Int]
    , nmPrev      :: Maybe ([Int], Int)
    , nmCurrent   :: Int
    , nmTotal     :: Int
    , nmNext      :: Maybe (Int, [Int])
    , nmNextJumps :: [Int]
    , nmLast      :: Bool
    }

-- | Given the number of items per page and the current page number, determine
-- the offset and limit.
subseq :: Int -> Int -> (Int, Int)
subseq per curr =
    let offset = (curr - 1) * per
        limit = per
    in  (offset, limit)

navModel :: NavSettings -> Int -> Int -> NavModel
navModel ns curr total = NavModel
    { nmFirst     = navEdges ns
    , nmPrevJumps = [] --TODO
    , nmPrev      =
        if curr == 1 || navNext ns < 1
            then Nothing
            else Just ([max 1 (curr - navNext ns) .. curr - 2], curr - 1)
    , nmCurrent   = curr
    , nmTotal     = total
    , nmNext      =
        if curr >= total || navNext ns < 1
            then Nothing
            else Just (curr + 1, [curr + 2 .. min total (curr + navNext ns)])
    , nmNextJumps = [] --TODO
    , nmLast      = navEdges ns
    }

paginate
    :: Monad m
    => PaginateSettings m f i
    -> NavSettings
    -> Int
    -> m (Int, Int, f i, NavModel)
paginate ps ns curr = do
    let (offset, limit) = subseq (psPer ps) curr
    (total, items) <- psSelect ps offset limit
    let pages =
            let (d, m) = total `divMod` psPer ps
            in  if m == 0 then d else d + 1
    return (total, pages, items, navModel ns curr pages)


-- | Get a page's contents and its navigation controls.
paginateMaybe
    :: Monad m
    => PaginateSettings m f i
    -- ^ How to get the page contents and split them into pages
    -> NavSettings
    -- ^ How to build page navigation controls for the user interface
    -> m (Maybe (Int, Int, f i, NavModel))
    -- ^ The items in the current page, and the navigation controls
paginateMaybe ps ns = do
    mcurr <- psCurrent ps
    traverse (paginate ps ns) mcurr

paginateCount
    :: Monad m
    => PaginateSettings m f i
    -> NavSettings
    -> m Int
    -> m (Int, Int, Maybe (f i, NavModel))
paginateCount ps ns count = do
    mresult <- paginateMaybe ps ns
    case mresult of
        Nothing -> do
            total <- count
            let pages =
                    let (d, m) = total `divMod` psPer ps
                    in  if m == 0 then d else d + 1
            return (total, pages, Nothing)
        Just (total, pages, items, nav) ->
            return (total, pages, Just (items, nav))

paginateTop
    :: Monad m
    => PaginateSettings m f i
    -> NavSettings
    -> m (Int, Int, f i, NavModel)
paginateTop ps ns = do
    curr <- fromMaybe 1 <$> psCurrent ps
    paginate ps ns curr
