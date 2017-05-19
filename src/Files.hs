{-# LANGUAGE OverloadedStrings #-}

module Files where

import           Types
--import Text.Regex
-- import Data.CSV
-- import Data.T.Text.Utils
import           Data.List
import           Data.Maybe
import qualified Data.Text   as T
import           Debug.Trace
import           Safe

toRows :: File -> ImportHeader -> [Column] -> [Row]
toRows [] _ _ = []
toRows _ _ [] = []
toRows (fileLine : rest) header clmns = toRowsHelper fileLine header clmns : toRows rest header clmns

toRowsHelper :: ImportRow -> ImportHeader -> [Column] -> Row
toRowsHelper row header (c:restColumns) = (c, getColumnValue row header c) : toRowsHelper row header restColumns
toRowsHelper _ _ [] = []

getColumnValue :: ImportRow -> ImportHeader -> Column -> T.Text
getColumnValue row header column = at row $ fromMaybe 0  (elemIndex (importName column) header)

fromRows :: [[Row]] -> [File]
fromRows = fmap fromRowsHelper

fromRowsHelper :: [Row] -> File
fromRowsHelper rowList = printHeader rowList : fmap printRow rowList

printHeader :: [Row] -> ImportRow
printHeader []      = [""]
printHeader (row:_) = fmap (\(column, _) -> exportName column) row

printRow :: Row -> ImportRow
printRow = fmap snd
