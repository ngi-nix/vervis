{- This file is part of Vervis.
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

module Language.Haskell.TH.Quote.Local
    ( expQuasiQuoter
    , decQuasiQuoter
    )
where

import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Q, Exp, Dec)

expQuasiQuoter :: (String -> Q Exp) -> QuasiQuoter
expQuasiQuoter qe = QuasiQuoter
    { quoteExp  = qe
    , quotePat  = err
    , quoteType = err
    , quoteDec  = err
    }
    where
    err = error "This quasi quoter is only for generating expressions"

decQuasiQuoter :: (String -> Q [Dec]) -> QuasiQuoter
decQuasiQuoter qd = QuasiQuoter
    { quoteExp  = err
    , quotePat  = err
    , quoteType = err
    , quoteDec  = qd
    }
    where
    err = error "This quasi quoter is only for generating declarations"
