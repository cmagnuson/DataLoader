{-# LANGUAGE DeriveGeneric #-}

module Types where

import           Data.Aeson
import qualified Data.Text    as T
import           Data.Tree
import           GHC.Generics

data Column = Column {
     importName :: T.Text,
     exportName :: T.Text
} deriving (Eq, Show, Generic)


newtype Filter = Filter (FilterExplanation, FilterOp)
  deriving(Generic)

data ImportDefinition = ImportDefinition {
     columns :: [Column],
     filters :: [Filter]
} deriving(Generic)

mkCol :: T.Text -> Column
mkCol s = Column s s

type Row = [(Column, Maybe T.Text)]

type ImportRow = [T.Text]

type ImportHeader = ImportRow

type ImportFile = [ImportRow]

type File = [Row]

type Fileset = Tree (T.Text, File)

type FilterOp = Fileset -> Fileset

type FilterExplanation = Fileset -> [T.Text]
