{- This file is part of persistent-migration.
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

{-# LANGUAGE OverloadedStrings #-}

module Database.Persist.Schema.Parser
    ( -- * Error types
      ModelError (..)
    , ErrorBundle
      -- * Name types
    , FieldName ()
    , renderFieldName
    , fieldNameDB
    , UniqueName ()
    , renderUniqueName
    , uniqueNameDB
    , EntityName ()
    , renderEntityName
    , entityNameDB
      -- * Schema types
    , FieldType (..)
    , FieldMaybe (..)
    , Field (..)
    , Unique (..)
    , Entity (..)
      -- * Schema parsing
    , parseEntities
    , parseEntitiesExt
    , parseEntitiesTest
    , parseEntitiesExtTest
    )
where

import Data.Char
import Data.Either
import Data.Functor
import Data.Maybe
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import Data.Text (Text)
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Control.Monad.Combinators.NonEmpty as CNE
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

data EntityError
    = FieldNameRepeated FieldName
    | UniqueNameRepeated UniqueName
    | UniqueFieldDoesntExist UniqueName FieldName
    | EntityNameRepeated
    | EntityNameTaken
    | FieldRefTargetDoesntExist FieldName EntityName
    deriving (Eq, Ord, Show)

data ModelError = EntityError EntityName EntityError deriving (Eq, Ord, Show)

instance ShowErrorComponent ModelError where
    showErrorComponent (EntityError ent err) =
        T.unpack $
            T.concat
                ["Entity ‘", renderEntityName ent, "’: ", showEntityError err]
        where
        showEntityError (FieldNameRepeated fld) =
            T.concat
                [ "Field ‘"
                , renderFieldName fld
                , "’ occurs multiple times"
                ]
        showEntityError (UniqueNameRepeated unq) =
            T.concat
                [ "Unique constraint ‘"
                , renderUniqueName ent unq
                , "’ occurs multiple times"
                ]
        showEntityError (UniqueFieldDoesntExist unq fld) =
            T.concat
                [ "Unique contraint ‘"
                , renderUniqueName ent unq
                , "’ refers to a field ‘"
                , renderFieldName fld
                , "’, but the entity has no field by this name"
                ]
        showEntityError EntityNameRepeated =
            "Multiple entities by this name"
        showEntityError EntityNameTaken =
            "There is already an entity by this name"
        showEntityError (FieldRefTargetDoesntExist fld target) =
            T.concat
                [ "Field ‘"
                , renderFieldName fld
                , "’ is a foreign reference to entity ‘"
                , renderEntityName target
                , "’, but no such entity exists"
                ]

_modelError :: ModelError -> Parser a
_modelError = customFailure

modelErrors :: NonEmpty ModelError -> Parser a
modelErrors = fancyFailure . S.fromList . NE.toList . NE.map ErrorCustom

type Parser = Parsec ModelError Text

type ErrorBundle = ParseErrorBundle Text ModelError

lineComment :: Parser ()
lineComment = L.skipLineComment "--"

blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

{-
midSpace1 :: Parser ()
midSpace1 = void $ takeWhile1P Nothing isLineSpace
    where
    isLineSpace c = c == ' ' || c == '\t'
-}

lineSpace1 :: Parser ()
lineSpace1 = L.space (void $ takeWhile1P Nothing isLineSpace) lineComment empty
    where
    isLineSpace c = c == ' ' || c == '\t'

blockSpace1 :: Parser ()
blockSpace1 = L.space space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme lineSpace1

data Segment = Segment
    { _segmentHeadUpper :: Char
    , _segmentHeadLower :: Char
    , _segmentTail      :: Text
    }
    deriving (Eq, Ord)

renderSegment :: Segment -> Text
renderSegment (Segment c _ t) = T.cons c t

renderSegmentLow :: Segment -> Text
renderSegmentLow (Segment _ c t) = T.cons c t

segment :: Parser Segment
segment =
    seg <$> satisfy isAsciiUpper
        <*> takeWhileP Nothing isSegChar
    where
    seg c t = Segment c (toLower c) t
    isSegChar c = isAsciiLower c || isDigit c

segmentLow :: Parser Segment
segmentLow =
    seg <$> satisfy isAsciiLower
        <*> takeWhileP Nothing isSegChar
    where
    seg c t = Segment (toUpper c) c t
    isSegChar c = isAsciiLower c || isDigit c

typeName :: Parser (NonEmpty (NonEmpty Segment))
typeName = CNE.sepBy1 (CNE.some segment) (char '.')

newtype FieldName = FieldName (NonEmpty Segment) deriving (Eq, Ord)

instance Show FieldName where
    show fld =
        T.unpack $ T.concat ["FieldName \"", renderFieldName fld, "\""]

renderFieldName :: FieldName -> Text
renderFieldName (FieldName (seg :| segs)) =
    T.concat $ renderSegmentLow seg : map renderSegment segs

fieldNameDB :: FieldName -> Text
fieldNameDB (FieldName segs) =
    T.intercalate "_" $ NE.toList $ NE.map renderSegmentLow segs

fieldNam :: Parser FieldName
fieldNam = f <$> segmentLow <*> many segment
    where
    f h t = FieldName $ h :| t

newtype UniqueName = UniqueName [Segment] deriving (Eq, Ord)

instance Show UniqueName where
    show unq =
        T.unpack $ T.concat ["UniqueName \"", renderUniqueName ent' unq, "\""]
        where
        ent' = EntityName $ Segment 'E' 'e' "ntity" :| []

renderUniqueName :: EntityName -> UniqueName -> Text
renderUniqueName ent (UniqueName segs) =
    T.concat
        ["Unique", renderEntityName ent, T.concat $ map renderSegment segs]

uniqueNameDB :: EntityName -> UniqueName -> Text
uniqueNameDB ent (UniqueName segs) =
    T.intercalate "_" $ "unique" : entityNameDB ent : map renderSegmentLow segs

uniqueNam :: Text -> Parser UniqueName
uniqueNam ent =
    string "Unique" *> string ent *> (UniqueName <$> many segment)

newtype EntityName = EntityName (NonEmpty Segment) deriving (Eq, Ord)

instance Show EntityName where
    show ent =
        T.unpack $ T.concat ["EntityName \"", renderEntityName ent, "\""]

renderEntityName :: EntityName -> Text
renderEntityName (EntityName segs) =
    T.concat $ NE.toList $ NE.map renderSegment segs

entityNameDB :: EntityName -> Text
entityNameDB (EntityName segs) =
    T.intercalate "_" $ NE.toList $ NE.map renderSegmentLow segs

entityNam :: Parser EntityName
entityNam = EntityName <$> CNE.some segment

data FieldType = FieldTypeRef EntityName | FieldTypePrim Text deriving Show

fieldTyp :: Parser FieldType
fieldTyp = do
    (nameText, modules) <- match typeName
    return $
        case modules of
            segs :| [] ->
                let i = NE.init segs
                    l = NE.last segs
                in  case (NE.nonEmpty i, l == idSeg) of
                        (Just i', True) -> FieldTypeRef $ EntityName i'
                        _               -> FieldTypePrim nameText
            _ -> FieldTypePrim nameText
    where
    idSeg = Segment 'I' 'i' "d"

data FieldMaybe = FieldRequired | FieldOptional deriving Show

data Field = Field
    { fieldName  :: FieldName
    , fieldType  :: FieldType
    , fieldMaybe :: FieldMaybe
    }
    deriving Show

field :: Parser Field
field = Field
    <$> lexeme fieldNam
    <*> lexeme fieldTyp
    <*> (fieldMayb <$> optional (lexeme $ string "Maybe"))
    where
    fieldMayb Nothing  = FieldRequired
    fieldMayb (Just _) = FieldOptional

data Unique = Unique
    { uniqueName   :: UniqueName
    , uniqueFields :: NonEmpty FieldName
    }
    deriving Show

unique :: Text -> Parser Unique
unique ent = Unique
    <$> lexeme (uniqueNam ent)
    <*> CNE.some (lexeme fieldNam)

data Entity = Entity
    { entityName    :: EntityName
    , entityFields  :: [Field]
    , entityUniques :: [Unique]
    }
    deriving Show

entity :: Parser Entity
entity = L.nonIndented blockSpace1 $ L.indentBlock blockSpace1 $ do
    (nameText, name) <- lexeme $ match entityNam
    return $ L.IndentMany Nothing (mkEntity name) (child nameText)
    where
    child ent = Left <$> lexeme field <|> Right <$> lexeme (unique ent)
    mkEntity name children = do
        let (fields, uniques) = partitionEithers children
            fieldNames = sort $ map fieldName fields
            uniqueNames = sort $ map uniqueName uniques
            fieldSet = S.fromAscList fieldNames

            repeatedFieldNames =
                map FieldNameRepeated $ findRepeated fieldNames
            repeatedUniqueNames =
                map UniqueNameRepeated $ findRepeated uniqueNames
            invalidUniqueFields =
                concatMap (checkUnique fieldSet) uniques

            allErrors =
                repeatedFieldNames ++
                repeatedUniqueNames ++
                invalidUniqueFields

        case NE.nonEmpty allErrors of
            Nothing -> return $ Entity name fields uniques
            Just errs -> modelErrors $ NE.map (EntityError name) errs
        where
        findRepeated :: Eq a => [a] -> [a]
        findRepeated = map NE.head . filter (not . null . NE.tail) . NE.group
        checkUnique fns (Unique n ps) =
            map (UniqueFieldDoesntExist n) $ NE.filter (`S.notMember` fns) ps

model :: Maybe (Set EntityName) -> Parser [Entity]
model oldEntitySetMabe = do
    entities <- blockSpace1 *> many entity <* eof

    let newEntityNames = sort $ map entityName entities
        newEntitySet = S.fromAscList newEntityNames

        repeatedEntityNames =
            map (flip EntityError EntityNameRepeated) $
            findRepeated newEntityNames
        takenEntityNames =
            case oldEntitySetMabe of
                Nothing -> []
                Just oldEntitySet ->
                    map (flip EntityError EntityNameTaken) $
                    S.toList $ S.intersection oldEntitySet newEntitySet
        inexistentRefs =
            case oldEntitySetMabe of
                Nothing -> []
                Just oldEntitySet ->
                    let allEntitiesSet = oldEntitySet `S.union` newEntitySet
                    in  concatMap (findInexistentRefs allEntitiesSet) entities

        allErrors = repeatedEntityNames ++ takenEntityNames ++ inexistentRefs

    case NE.nonEmpty allErrors of
        Nothing -> return entities
        Just errs -> modelErrors errs
    where
    findRepeated = map NE.head . filter (not . null . NE.tail) . NE.group
    findInexistentRefs ents (Entity ent fields _) =
        map (EntityError ent) $ mapMaybe inexistentTarget fields
        where
        inexistentTarget (Field fld (FieldTypeRef target) _)
            | target `S.notMember` ents =
                Just $ FieldRefTargetDoesntExist fld target
        inexistentTarget _ = Nothing

parseEntities :: FilePath -> Text -> Either ErrorBundle [Entity]
parseEntities = parseEntitiesExt $ Just S.empty

parseEntitiesExt
    :: Maybe (Set EntityName)
    -> FilePath
    -> Text
    -> Either ErrorBundle [Entity]
parseEntitiesExt olds = parse $ model olds

parseEntitiesTest :: Text -> IO ()
parseEntitiesTest = parseEntitiesExtTest $ Just S.empty

parseEntitiesExtTest :: Maybe (Set EntityName) -> Text -> IO ()
parseEntitiesExtTest olds = parseTest $ model olds
