{-# LANGUAGE OverloadedStrings #-}

module Filters where

import qualified Data.List   as L
import           Data.Monoid
import qualified Data.Text   as T
import           Types

-- partial string to find and replacement string for given column
findAndReplace :: T.Text -> T.Text -> Column -> FilterOp
-- for each [Row] check each Row for String replacement
                                          -- each row list, each row, each (Column, String)
findAndReplace oldString newString column = fmap (fmap (fmap (findReplaceHelper oldString newString column)))

findReplaceHelper :: T.Text -> T.Text -> Column -> (Column, T.Text) -> FilterOp
findReplaceHelper oldString newString column (testColumn, str)
                  | column == testColumn = (column, T.replace oldString newString str)
                  | otherwise = (testColumn, str)

-- matching string and column to delete on
delete :: T.Text -> Column -> FilterOp
delete string column = fmap (deleteIf (\row -> getColumnValue column row == Just string))


--sortAsc :: Column -> FilterOp
--FIXME!
--sortAsc column = \x -> fmap (sort (getColumnValue column)) x

--sortDsc :: Column -> FilterOp
--sortDsc column = \x -> fmap reverse (sortAsc column x)

--TODO: add me!
-- split :: Column -> FilterOp

getColumnValue :: Column -> Row -> Maybe T.Text
getColumnValue testCol ((col, str) : rest)
               | col == testCol = Just str
               | null rest = Nothing
               | otherwise = getColumnValue testCol rest
getColumnValue _ [] = Nothing

-- helper to delete rows based on given predicate
deleteIf                :: (a -> Bool) -> [a] -> [a]
deleteIf _  []     = []
deleteIf eq (y:ys) = if eq y then deleteIf eq ys else y : deleteIf eq ys

deleteIfContains :: T.Text -> Column -> FilterOp
deleteIfContains string column = fmap (deleteIf (\row -> containsString string (getColumnValue column row )))

containsString :: T.Text -> Maybe T.Text -> Bool
containsString _ Nothing                = False
containsString testString (Just string) = T.isInfixOf testString string

fileSplitOnColumn :: Column -> FilterOp
fileSplitOnColumn col = L.concat . fmap (\row -> (fileSplitOnColumnHelper col row))

fileSplitOnColumnHelper :: Column -> [Row] -> [[Row]]
fileSplitOnColumnHelper col rows = L.groupBy (\a b -> ((getColumnValue col a) == (getColumnValue col b))) rows



logFilteredCount :: FilterOp -> T.Text -> [[Row]] -> [T.Text]
logFilteredCount op name rows = ["Filtering " <> name <> (show (countRows rows - countRows (op rows)))]

countRows :: [[Row]] -> Int
countRows rows = length $ concat rows
