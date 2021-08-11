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

module Vervis.Widget.Project
    ( projectNavW
    )
where

import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident
import Vervis.Settings
import Vervis.Widget.Workflow

projectNavW :: Project -> Workflow -> Sharer -> ShrIdent -> PrjIdent -> Widget
projectNavW project workflow wsharer shar proj =
    $(widgetFile "project/widget/nav")
