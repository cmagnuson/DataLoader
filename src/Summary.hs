{-# LANGUAGE OverloadedStrings #-}

module Summary (countColumnUniqueValues, countColumnDuplicateValues) where

import qualified Data.List   as L
import           Data.Map    (fromListWith, toList)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text   as T
import           Files       (flattenIgnoreEmpty)
import           Filters
import           Types

countColumnUniqueValues :: Column -> Filter
countColumnUniqueValues column = (countColumnUniqueValuesLogger column, id)

countColumnUniqueValuesLogger :: Column -> Fileset -> [T.Text]
countColumnUniqueValuesLogger column rows = ["Column " <> exportName column <> T.pack " values: " <>
          countColumnUniqueValuesPrintHelper (L.group $ L.sort (fmap (fromMaybe "" . getColumnValue column) (L.concat $ fmap snd rows)))]
          where
            -- List of list of grouped values [[1,1,1], [2,2], [F,F]] to count and unique value: (1,3), (2,2), (F,2) for example
            countColumnUniqueValuesPrintHelper values =
              T.intercalate  (T.pack ", ") (fmap (\lst -> T.pack "(" <> head lst <> T.pack  "," <> T.pack  (show $ length lst) <> T.pack  ")") values)

countColumnDuplicateValues :: Column -> Filter
countColumnDuplicateValues column = (countColumnDuplicateValuesLogger column, id)

countColumnDuplicateValuesLogger :: Column -> Fileset -> [T.Text]
countColumnDuplicateValuesLogger column rows = ["Column " <> exportName column <> T.pack "duplicate values: " <>
        countColumnUniqueValuesPrintHelper fileFreq]
          where
            fileFreq = frequencyCountFileset rows column
            -- List of list of grouped values [[1,1,1], [2,2], [F,F]] to count and unique value: (1,3), (2,2), (F,2) for example
            countColumnUniqueValuesPrintHelper :: [(T.Text, [(T.Text, Integer)])] -> T.Text
            countColumnUniqueValuesPrintHelper [] = ""
            countColumnUniqueValuesPrintHelper ((filename, counts):rest) = printSingleFile filename (filter (\(_, count) -> count > 1) counts) <> countColumnUniqueValuesPrintHelper rest
            printSingleFile :: T.Text -> [(T.Text, Integer)] -> T.Text
            printSingleFile _ []        = ""
            printSingleFile name counts = name <> ": " <> T.intercalate "," (fmap (\(text, count) -> "(" <> text <> "," <> (T.pack $ show count) <> ")") counts)

frequencyCount :: File -> Column -> [(T.Text, Integer)]
frequencyCount file column = (\xs -> toList (fromListWith (+) [(x, 1) | x <- xs])) $ fmap (fromMaybe "" . getColumnValue column) file

frequencyCountFileset :: Fileset -> Column -> [(T.Text, [(T.Text, Integer)])]
frequencyCountFileset fileset column = fmap (\file -> ((fst file), frequencyCount (snd file) column)) (flattenIgnoreEmpty fileset)
