{-# LANGUAGE OverloadedStrings #-}

module Json where

import           Data.Aeson
import qualified Data.Text  as T
import           Filters
import           Summary
import           Types


instance FromJSON ImportDefinition
instance ToJSON ImportDefinition

instance FromJSON Column where
 parseJSON (Object v) = mkColMay <$>
                        v .: "importName" <*>
                        v .:? "exportName"
instance ToJSON Column

mkColMay :: T.Text -> Maybe T.Text -> Column
mkColMay s Nothing   = mkCol s
mkColMay s (Just s2) = Column s s2

instance FromJSON Filter where
   parseJSON = withObject "filter" $ \o -> do
     filterName <- o .: "filter"
     case filterName of
       "stripSpecialChars"            -> return stripSpecialCharsFilter
       "findAndReplace"               -> findAndReplaceFilter <$> o .: "oldString" <*> o .: "newString" <*> (mkCol <$> (o .: "column"))
       "delete"                       -> deleteFilter <$> o .: "value" <*> (mkCol <$> (o .: "column"))
       "deleteIfContains"             -> deleteIfContainsFilter <$> o .: "value" <*> (mkCol <$> (o .: "column"))
       "fileSplitOnColumn"            -> fileSplitOnColumnFilter <$> (mkCol <$> (o .: "column"))
       "fileSplitOnColumnEquals"      -> fileSplitOnColumnEqualsFilter <$> (mkCol <$> (o .: "column")) <*> o .: "value" <*> o.: "fileSuffix"
       "countColumnUniqueValues"      -> countColumnUniqueValues <$> (mkCol <$> (o .: "column"))
       "countColumnDuplicateValues"   -> countColumnDuplicateValues <$> (mkCol <$> (o .: "column"))
       _                              -> fail ("unknown filter: " ++ filterName)
instance ToJSON Filter where
    toJSON _ = "" --TODO: implement serialization as well?  Need new data type to tag how to serialize if so

_simpleJson :: Maybe ImportDefinition
_simpleJson = decode "{\"columns\":[{\"importName\":\"a\"}],\"filters\":[{\"filter\":\"findAndReplace\", \"oldString\":\"a\", \"newString\":\"a\", \"column\":\"a\"}]}"
