{-# LANGUAGE OverloadedStrings #-}

module Filters where

-- import qualified Data.List   as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text   as T
import           Types

-- partial string to find and replacement string for given column
findAndReplace :: T.Text -> T.Text -> Column -> FilterOp
-- for each [Row] check each Row for String replacement
                                          -- each row list, each row, each (Column, String)
findAndReplace oldString newString column = fmap (fmap (fmap (findReplaceHelper oldString newString column)))
  where findReplaceHelper old new col (testColumn, strM)
                  | col == testColumn = (col, Just $ T.replace old new str)
                  | otherwise = (testColumn, Just str)
                  where
                    str = fromMaybe "" strM


deleteFilter :: T.Text -> Column -> Filter
deleteFilter string column =
    let op = delete string column
    in (logFilteredCount op  ("Delete where " <> (exportName column) <> " == " <> string), op)

-- matching string and column to delete on
delete :: T.Text -> Column -> FilterOp
delete string column = fmap (deleteIf (\row -> getColumnValue column row == Just string))


getColumnValue :: Column -> Row -> Maybe T.Text
getColumnValue testCol ((col, strM) : rest)
               | col == testCol = Just str
               | null rest = Nothing
               | otherwise = getColumnValue testCol rest
               where
                   str = fromMaybe "" strM
getColumnValue _ [] = Nothing

-- helper to delete rows based on given predicate
deleteIf                :: (a -> Bool) -> [a] -> [a]
deleteIf _  []     = []
deleteIf eq (y:ys) = if eq y then deleteIf eq ys else y : deleteIf eq ys

deleteIfContainsFilter :: T.Text -> Column -> Filter
deleteIfContainsFilter string column =
    let op = deleteIfContains string column
    in (logFilteredCount op  ("Delete where " <> (exportName column) <> " contains " <> string), op)

deleteIfContains :: T.Text -> Column -> FilterOp
deleteIfContains string column = fmap (deleteIf (\row -> containsString string (getColumnValue column row )))

containsString :: T.Text -> Maybe T.Text -> Bool
containsString _ Nothing                = False
containsString testString (Just string) = T.isInfixOf testString string

-- fileSplitOnColumnFilter :: Column -> Filter
-- fileSplitOnColumnFilter column =
--     let op = fileSplitOnColumn column
--     in (logFilesCount op (exportName column), op)
--
-- fileSplitOnColumnEqualsFilter :: Column -> T.Text -> Filter
-- fileSplitOnColumnEqualsFilter column value =
--     let op = fileSplitOnColumnEquals column value
--     in (logFilesCount op (exportName column <> "==" <> value), op)
--
-- fileSplitOnColumn :: Column -> FilterOp
-- fileSplitOnColumn column = L.concat . fmap (\row -> (fileSplitOnColumnHelper column row))
--   where fileSplitOnColumnHelper col rows = L.transpose $ L.groupBy (\a b -> ((getColumnValue col a) == (getColumnValue col b))) rows
--
-- fileSplitOnColumnEquals :: Column -> T.Text -> FilterOp
-- fileSplitOnColumnEquals column string =
--   L.concat . fmap (\row -> dePair $ fileSplitOnColumnValueEquals column (Just string) row)
--   where
--     fileSplitOnColumnValueEquals eqCol str = L.partition (\row -> getColumnValue eqCol row == str)
--     dePair (a,b) = [a,b]

logFilesCount :: FilterOp -> T.Text -> Fileset -> [T.Text]
logFilesCount op name rows = ["Splitting on " <> name <> " " <> (T.pack $ show $ length rows) <> " -> " <> (T.pack $ show $ length $ op rows) <> " files "
                              <> (T.pack $ show $ fmap length $ op rows)]

logFilteredCount :: FilterOp -> T.Text -> Fileset -> [T.Text]
logFilteredCount op name rows = [name <> ": " <> (T.pack $ show (countRows rows - countRows (op rows)))]

countRows :: Fileset -> Int
countRows rows = length $ concat rows
