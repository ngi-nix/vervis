{- This file is part of Vervis.
 -
 - Written in 2016 by fr33domlover <fr33domlover@riseup.net>.
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

-- | File content types and tools for detecting them. The focus is on content
-- that gets special treatment in Vervis, and not general MIME type modeling or
-- detection (although that could be done in the future).
module Data.MediaType
    ( MediaType (..)
    , FileName
    , FileBaseName
    , FileExtension
    , WorkType
    , SourceViewOptions
    , chooseMediaType
    )
where

import Data.Text (Text)

data MediaType
    = PlainText
    | XML
    | JSON
    | YAML
    | HTML
    | CSS
    | Markdown
    | CSource
    | CHeader
    | Haskell
    | LiterateHaskell
    | CabalPackageDescription
    | PersistentTemplate
    | YesodRouteTemplate
    | Hamlet
    | Cassius
    deriving Show

type FileName = Text

type FileBaseName = Text

type FileExtension = Text

type WorkType = ()

type SourceViewOptions = ()

chooseMediaType
    :: [FileName]
    -> FileBaseName
    -> FileExtension
    -> WorkType -- project type
    -> SourceViewOptions -- e.g. whether to see rendered pages or their sources
    -> MediaType
chooseMediaType dir base ext wt opts =
    case (dir, base, ext, wt) of
        -- * Data interchange
        (_, _, "xml"     , _) -> PlainText
        (_, _, "json"    , _) -> PlainText
        (_, _, "yml"     , _) -> PlainText
        (_, _, "yaml"    , _) -> PlainText
        -- * Documents
        (_, _, "txt"     , _) -> PlainText
        (_, _, "md"      , _) -> Markdown
        (_, _, "mdwn"    , _) -> Markdown
        (_, _, "mkdn"    , _) -> Markdown
        (_, _, "markdown", _) -> Markdown
        -- * Web page basics
        (_, _, "html"    , _) -> PlainText
        (_, _, "xhtml"   , _) -> PlainText
        (_, _, "css"     , _) -> PlainText
        (_, _, "js"      , _) -> PlainText
        -- * Programming languages
        -- ** C
        (_, _, "c"       , _) -> PlainText
        (_, _, "h"       , _) -> PlainText
        -- ** C++
        (_, _, "cc"      , _) -> PlainText
        (_, _, "cpp"     , _) -> PlainText
        (_, _, "cxx"     , _) -> PlainText
        (_, _, "hh"      , _) -> PlainText
        (_, _, "hpp"     , _) -> PlainText
        -- ** Haskell
        (_, _, "hs"      , _) -> Haskell
        (_, _, "lhs"     , _) -> PlainText
        (_, _, "cabal"   , _) -> PlainText
        (_, _, "hamlet"  , _) -> PlainText
        (_, _, "cassius" , _) -> PlainText
        -- ** Java
        (_, _, "java"    , _) -> PlainText
        -- ** Lisp
        (_, _, "cl"      , _) -> PlainText
        (_, _, "el"      , _) -> PlainText
        -- ** Lua
        (_, _, "lua"     , _) -> PlainText
        -- ** Perl
        (_, _, "pl"      , _) -> PlainText
        -- ** PHP
        (_, _, "php"     , _) -> PlainText
        -- ** Python
        (_, _, "py"      , _) -> PlainText
        -- ** Ruby
        (_, _, "rb"      , _) -> PlainText
        -- ** Scheme
        (_, _, "scm"     , _) -> PlainText
        (_, _, _         , _) -> PlainText
