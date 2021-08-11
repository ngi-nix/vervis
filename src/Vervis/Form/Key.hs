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

module Vervis.Form.Key
    ( newKeyForm
    )
where

import Yesod.Form

import Vervis.Field.Key
import Vervis.Foundation
import Vervis.Model
import Vervis.Model.Ident

newKeyAForm :: PersonId -> AForm Handler SshKey
newKeyAForm pid = mk
    <$> (text2ky <$> areq (nameField pid) "Name*"     Nothing)
    <*> areq              sshKeyField     keySettings Nothing
    where
    mk n (a, c) = SshKey n pid a c
    keySettings =
        FieldSettings
            "Content*" Nothing Nothing Nothing [("placeholder", defKey)]
    defKey =
        "ssh-rsa \
        \AAAPcyBpcyubQgYBrZkWzZSBwY..."

newKeyForm :: PersonId -> Form SshKey
newKeyForm = renderDivs . newKeyAForm
