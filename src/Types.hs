{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Monad.Writer
import qualified Data.Text            as T
import           Data.Tree

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

type ImportFile = [ImportRow]

type File = [Row]

type Fileset = Tree File

type FilterOp = Fileset -> Fileset

type FilterExplanation = Fileset -> [T.Text]

type Filter = (FilterExplanation, FilterOp)

data ImportDefinition = ImportDefinition {
     columns :: [Column],
     filters :: [Filter]
}

filterRows :: Fileset -> [Filter] -> Writer [T.Text] Fileset
filterRows rows []            = return rows
filterRows rows ((filtExp, filtOp) : rest) = do
                                  tell $ filtExp rows
                                  filterRows (filtOp rows) rest
