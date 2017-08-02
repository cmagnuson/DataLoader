{-# LANGUAGE OverloadedStrings #-}

module Files where

import           Data.List
import           Data.Maybe
import qualified Data.Text  as T
import           Data.Tree  (flatten)
import           Safe
import           Types

toRows :: ImportFile -> ImportHeader -> [Column] -> [Row]
toRows [] _ _ = []
toRows _ _ [] = []
toRows (fileLine : rest) header clmns = toRowsHelper fileLine header clmns : toRows rest header clmns

toRowsHelper :: ImportRow -> ImportHeader -> [Column] -> Row
toRowsHelper row header (c:restColumns) = (c, getColumnValue row header c) : toRowsHelper row header restColumns
toRowsHelper _ _ [] = []

getColumnValue :: ImportRow -> ImportHeader -> Column -> Maybe T.Text
getColumnValue row header column = atMay row $ fromMaybe 0  (elemIndex (importName column) header)

fromRows :: Fileset -> [ImportFile]
fromRows fileset = flatten $ fmap fromRowsHelper fileset

fromRowsHelper :: File -> ImportFile
fromRowsHelper rowList = printHeader rowList : fmap printRow rowList

printHeader :: [Row] -> ImportRow
printHeader []      = [""]
printHeader (row:_) = fmap (\(column, _) -> exportName column) row

printRow :: Row -> ImportRow
printRow = fmap (fromMaybe "" . snd)

fileToString :: ImportFile -> String
fileToString file = intercalate "\r\n" $ fmap importRowToString file

importRowToString :: ImportRow -> String
importRowToString row = intercalate "," $ fmap show row
