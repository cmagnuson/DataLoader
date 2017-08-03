{-# LANGUAGE OverloadedStrings #-}

module Summary where

import qualified Data.List   as L
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text   as T
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
