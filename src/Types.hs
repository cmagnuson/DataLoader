{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Monad.Writer
import qualified Data.Text            as T

data Column = Column
    {
     importName :: T.Text,
     exportName :: T.Text
    } deriving (Eq, Show)

mkCol :: T.Text -> Column
mkCol s = Column s s

type Row = [(Column, Maybe T.Text)]

type ImportRow = [T.Text]

type ImportHeader = ImportRow

type File = [ImportRow]

type FilterOp = [[Row]] -> [[Row]]

type FilterExplanation = [[Row]] -> [T.Text]

type Filter = (FilterExplanation, FilterOp)

data ImportDefinition = ImportDefinition {
     columns :: [Column],
     filters :: [Filter]
}

filterRows :: [[Row]] -> [Filter] -> Writer [T.Text] [[Row]]
filterRows  [] _              = return []
filterRows rows []            = return rows
filterRows rows ((filtExp, filtOp) : rest) = do
                                  tell $ filtExp rows
                                  filterRows (filtOp rows) rest
