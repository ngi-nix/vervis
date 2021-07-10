{- This file contains (slightly modified) copies of unexported functions from
 - Database.Persist.Sql.Orphan.PersistQuery, which I need for my
 - PersistQueryRecursive implementation. They're released under MIT.
 -
 - This should be a temporary situation. Either my code moves to persistent and
 - the functions are reused there, or these functions become exported in
 - persistent and then I can import them instead of holding copies.
 -
 - Copyright (c) 2012 Michael Snoyman, http://www.yesodweb.com/
 - Modified in 2016 by fr33domlover.
 -
 - Permission is hereby granted, free of charge, to any person obtaining
 - a copy of this software and associated documentation files (the
 - "Software"), to deal in the Software without restriction, including
 - without limitation the rights to use, copy, modify, merge, publish,
 - distribute, sublicense, and/or sell copies of the Software, and to
 - permit persons to whom the Software is furnished to do so, subject to
 - the following conditions:
 -
 - The above copyright notice and this permission notice shall be
 - included in all copies or substantial portions of the Software.
 -
 - THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 - EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 - MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 - NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 - LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 - OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 - WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Database.Persist.Local.Sql.Orphan.Common
    ( fieldName
    , dummyFromFilts
    , getFiltsValues
    , filterClause
    , orderClause
    )
where

import Data.List (inits, transpose)
import Data.Monoid ((<>))
import Data.Text (Text)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sql.Util

import qualified Data.Text as T

fieldName
    :: forall record typ.
        (PersistEntity record , PersistEntityBackend record ~ SqlBackend)
    => EntityField record typ
    -> DBName
fieldName f = fieldDB $ persistFieldDef f

dummyFromFilts :: [Filter v] -> Maybe v
dummyFromFilts _ = Nothing

getFiltsValues
    :: forall val. (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
    => SqlBackend
    -> [Filter val]
    -> [PersistValue]
getFiltsValues conn = snd . filterClauseHelper False False conn OrNullNo

data OrNull = OrNullYes | OrNullNo

filterClauseHelper
    :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
    => Bool -- ^ include table name?
    -> Bool -- ^ include WHERE?
    -> SqlBackend
    -> OrNull
    -> [Filter val]
    -> (Text, [PersistValue])
filterClauseHelper includeTable includeWhere conn orNull filters =
    ( if not (T.null sql) && includeWhere
        then " WHERE " <> sql
        else sql
    , vals
    )
  where
    (sql, vals) = combineAND filters
    combineAND = combine " AND "

    combine s fs =
        (T.intercalate s $ map wrapP a, mconcat b)
      where
        (a, b) = unzip $ map go fs
        wrapP x = T.concat ["(", x, ")"]

    go (BackendFilter _) = error "BackendFilter not expected"
    go (FilterAnd []) = ("1=1", [])
    go (FilterAnd fs) = combineAND fs
    go (FilterOr []) = ("1=0", [])
    go (FilterOr fs)  = combine " OR " fs
    go (Filter field value pfilter) = 
        let t = entityDef $ dummyFromFilts [Filter field value pfilter]
        in case (isIdField field, entityPrimary t, allVals) of
                 (True, Just pdef, PersistList ys:_) ->
                    if length (compositeFields pdef) /= length ys
                       then error $ "wrong number of entries in compositeFields vs PersistList allVals=" ++ show allVals
                    else
                      case (allVals, pfilter, isCompFilter pfilter) of
                        ([PersistList xs], Eq, _) -> 
                           let sqlcl=T.intercalate " and " (map (\a -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "? ")  (compositeFields pdef))
                           in (wrapSql sqlcl,xs)
                        ([PersistList xs], Ne, _) -> 
                           let sqlcl=T.intercalate " or " (map (\a -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "? ")  (compositeFields pdef))
                           in (wrapSql sqlcl,xs)
                        (_, In, _) -> 
                           let xxs = transpose (map fromPersistList allVals)
                               sqls=map (\(a,xs) -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "(" <> T.intercalate "," (replicate (length xs) " ?") <> ") ") (zip (compositeFields pdef) xxs)
                           in (wrapSql (T.intercalate " and " (map wrapSql sqls)), concat xxs)
                        (_, NotIn, _) -> 
                           let xxs = transpose (map fromPersistList allVals)
                               sqls=map (\(a,xs) -> connEscapeName conn (fieldDB a) <> showSqlFilter pfilter <> "(" <> T.intercalate "," (replicate (length xs) " ?") <> ") ") (zip (compositeFields pdef) xxs)
                           in (wrapSql (T.intercalate " or " (map wrapSql sqls)), concat xxs)
                        ([PersistList xs], _, True) -> 
                           let zs = tail (inits (compositeFields pdef))
                               sql1 = map (\b -> wrapSql (T.intercalate " and " (map (\(i,a) -> sql2 (i==length b) a) (zip [1..] b)))) zs
                               sql2 islast a = connEscapeName conn (fieldDB a) <> (if islast then showSqlFilter pfilter else showSqlFilter Eq) <> "? "
                               sqlcl = T.intercalate " or " sql1
                           in (wrapSql sqlcl, concat (tail (inits xs)))
                        (_, BackendSpecificFilter _, _) -> error "unhandled type BackendSpecificFilter for composite/non id primary keys"
                        _ -> error $ "unhandled type/filter for composite/non id primary keys pfilter=" ++ show pfilter ++ " persistList="++show allVals
                 (True, Just pdef, _) -> error $ "unhandled error for composite/non id primary keys pfilter=" ++ show pfilter ++ " persistList=" ++ show allVals ++ " pdef=" ++ show pdef

                 _ ->   case (isNull, pfilter, varCount) of
                            (True, Eq, _) -> (name <> " IS NULL", [])
                            (True, Ne, _) -> (name <> " IS NOT NULL", [])
                            (False, Ne, _) -> (T.concat
                                [ "("
                                , name
                                , " IS NULL OR "
                                , name
                                , " <> "
                                , qmarks
                                , ")"
                                ], notNullVals)
                            -- We use 1=2 (and below 1=1) to avoid using TRUE and FALSE, since
                            -- not all databases support those words directly.
                            (_, In, 0) -> ("1=2" <> orNullSuffix, [])
                            (False, In, _) -> (name <> " IN " <> qmarks <> orNullSuffix, allVals)
                            (True, In, _) -> (T.concat
                                [ "("
                                , name
                                , " IS NULL OR "
                                , name
                                , " IN "
                                , qmarks
                                , ")"
                                ], notNullVals)
                            (_, NotIn, 0) -> ("1=1", [])
                            (False, NotIn, _) -> (T.concat
                                [ "("
                                , name
                                , " IS NULL OR "
                                , name
                                , " NOT IN "
                                , qmarks
                                , ")"
                                ], notNullVals)
                            (True, NotIn, _) -> (T.concat
                                [ "("
                                , name
                                , " IS NOT NULL AND "
                                , name
                                , " NOT IN "
                                , qmarks
                                , ")"
                                ], notNullVals)
                            _ -> (name <> showSqlFilter pfilter <> "?" <> orNullSuffix, allVals) 

      where
        isCompFilter Lt = True
        isCompFilter Le = True
        isCompFilter Gt = True
        isCompFilter Ge = True
        isCompFilter _ =  False
        
        wrapSql sqlcl = "(" <> sqlcl <> ")"
        fromPersistList (PersistList xs) = xs
        fromPersistList other = error $ "expected PersistList but found " ++ show other
        
        filterValueToPersistValues :: forall a.  PersistField a => Either a [a] -> [PersistValue]
        filterValueToPersistValues v = map toPersistValue $ either return id v

        orNullSuffix =
            case orNull of
                OrNullYes -> mconcat [" OR ", name, " IS NULL"]
                OrNullNo -> ""

        isNull = any (== PersistNull) allVals
        notNullVals = filter (/= PersistNull) allVals
        allVals = filterValueToPersistValues value
        tn = connEscapeName conn $ entityDB
           $ entityDef $ dummyFromFilts [Filter field value pfilter]
        name =
            (if includeTable
                then ((tn <> ".") <>)
                else id)
            $ connEscapeName conn $ fieldName field
        qmarks = case value of
                    Left _ -> "?"
                    Right x ->
                        let x' = filter (/= PersistNull) $ map toPersistValue x
                         in "(" <> T.intercalate "," (map (const "?") x') <> ")"
        varCount = case value of
                    Left _ -> 1
                    Right x -> length x
        showSqlFilter Eq = "="
        showSqlFilter Ne = "<>"
        showSqlFilter Gt = ">"
        showSqlFilter Lt = "<"
        showSqlFilter Ge = ">="
        showSqlFilter Le = "<="
        showSqlFilter In = " IN "
        showSqlFilter NotIn = " NOT IN "
        showSqlFilter (BackendSpecificFilter s) = s

filterClause :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
             => Bool -- ^ include table name?
             -> SqlBackend
             -> [Filter val]
             -> Text
filterClause b c = fst . filterClauseHelper b True c OrNullNo

orderClause :: (PersistEntity val, PersistEntityBackend val ~ SqlBackend)
            => Bool -- ^ include the table name
            -> SqlBackend
            -> SelectOpt val
            -> Text
orderClause includeTable conn o =
    case o of
        Asc  x -> name x
        Desc x -> name x <> " DESC"
        _ -> error "orderClause: expected Asc or Desc, not limit or offset"
  where
    dummyFromOrder :: SelectOpt a -> Maybe a
    dummyFromOrder _ = Nothing

    tn = connEscapeName conn $ entityDB $ entityDef $ dummyFromOrder o

    name :: (PersistEntityBackend record ~ SqlBackend, PersistEntity record)
         => EntityField record typ -> Text
    name x =
        (if includeTable
            then ((tn <> ".") <>)
            else id)
        $ connEscapeName conn $ fieldName x
