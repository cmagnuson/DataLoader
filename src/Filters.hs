{-# LANGUAGE OverloadedStrings #-}

module Filters where

import           Data.Char   (isAlpha)
import qualified Data.List   as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text   as T
import           Data.Tree
import           Files       (flattenIgnoreEmpty)
import           Types

-- partial string to find and replacement string for given column
findAndReplace :: T.Text -> T.Text -> Column -> FilterOp
-- for each [Row] check each Row for String replacement
                                          -- each row list, each row, each (Column, String)
findAndReplace oldString newString column = fmap $ fmap (fmap (fmap (findReplaceHelper oldString newString column)))
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
delete string column = fmap $ fmap (deleteIf (\row -> getColumnValue column row == Just string))


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
deleteIfContains string column = fmap $ fmap (deleteIf (\row -> containsString string (getColumnValue column row )))

containsString :: T.Text -> Maybe T.Text -> Bool
containsString _ Nothing                = False
containsString testString (Just string) = T.isInfixOf testString string

-- We assume the tree has either rootLabel OR subForest, but not both
-- so Node {rootLabel=[1,2,3], subForest=[]} OR Node {rootLabel=[], subForest=[Node...]}
-- TODO: make a better tree data type which only allows a node value or subForest, not both
fileSplitGeneric :: (File -> [(T.Text, File)]) -> FilterOp
fileSplitGeneric splitter Node {rootLabel=(label,[]), subForest = forest} = Node {rootLabel=(label,[]), subForest = fmap (fileSplitGeneric splitter) forest}
fileSplitGeneric splitter Node {rootLabel=(label, file), subForest = _} =
  let files = splitter file
  in case files of []  -> Node {rootLabel=(label, file), subForest=[]}
                   [(_,f)] -> Node {rootLabel=(label, f), subForest=[]}
                   fs -> Node {rootLabel=(label,[]), subForest= fmap (\(suffix, x) -> Node{rootLabel=(label <> "_" <> suffix, x), subForest=[]}) fs}

fileSplitOnColumnFilter :: Column -> Filter
fileSplitOnColumnFilter column =
    let op = fileSplitOnColumn column
    in (logFilesCount op (exportName column), op)

fileSplitOnColumnEqualsFilter :: Column -> T.Text -> T.Text -> Filter
fileSplitOnColumnEqualsFilter column value fileSuffix =
    let op = fileSplitOnColumnEquals column value fileSuffix
    in (logFilesCount op (exportName column <> "==" <> value), op)

fileSplitOnColumn :: Column -> FilterOp
fileSplitOnColumn col = fileSplitGeneric (nameFiles 1 . L.transpose . L.groupBy (\a b -> (getColumnValue col a == getColumnValue col b)))
  where nameFiles :: Int -> [File] -> [(T.Text, File)]
        nameFiles count (file:rest) = (T.pack "split-" <> T.pack (filter isAlpha $ T.unpack $ exportName col) <> "-" <> T.pack (show count), file) : (nameFiles (count+1) rest)
        nameFiles _ [] = []

fileSplitOnColumnEquals :: Column -> T.Text -> T.Text -> FilterOp
fileSplitOnColumnEquals column string fileSuffix =
  fileSplitGeneric (\row -> dePair $ fileSplitOnColumnValueEquals column (Just string) row)
  where
    fileSplitOnColumnValueEquals eqCol str = L.partition (\row -> getColumnValue eqCol row == str)
    dePair (a,b) = [(fileSuffix, a),("NOT" <> fileSuffix, b)]

logFilesCount :: FilterOp -> T.Text -> Fileset -> [T.Text]
logFilesCount op name rows = ["Splitting on " <> name <> " " <> (T.pack $ show $ length $ flattenIgnoreEmpty  rows) <> " -> "
                              <> (T.pack $ show $ length $ flattenIgnoreEmpty $ op rows) <> " files "
                              <> (T.pack $ show $ (fmap (length . snd) $ flattenIgnoreEmpty(op rows)))]

logFilteredCount :: FilterOp -> T.Text -> Fileset -> [T.Text]
logFilteredCount op name rows = [name <> ": " <> (T.pack $ show (countRows rows - countRows (op rows)))]

countRows :: Fileset -> Int
countRows rows = length $ concat $ fmap snd rows
