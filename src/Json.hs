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
       "findAndReplace"               -> findAndReplaceFilter <$> o .: "oldString" <*> o .: "newString" <*> (mkColMay <$> (o .: "column") <*> o .:? "columnExport")
       "findAndReplaceExactMatch"               -> findAndReplaceExactMatchFilter <$> o .: "oldString" <*> o .: "newString" <*> (mkColMay <$> (o .: "column") <*> o .:? "columnExport")
       "delete"                       -> deleteFilter <$> o .: "value" <*> (mkColMay <$> (o .: "column") <*> o .:? "columnExport")
       "deleteIfContains"             -> deleteIfContainsFilter <$> o .: "value" <*> (mkColMay <$> (o .: "column") <*> o .:? "columnExport")
       "fileSplitOnColumn"            -> fileSplitOnColumnFilter <$> (mkColMay <$> (o .: "column") <*> o .:? "columnExport")
       "fileSplitOnColumnEquals"      -> fileSplitOnColumnEqualsFilter <$> (mkColMay <$> (o .: "column") <*> o .:? "columnExport") <*> o .: "value" <*> o.: "fileSuffix"
       "countColumnUniqueValues"      -> countColumnUniqueValues <$> (mkColMay <$> (o .: "column") <*> o .:? "columnExport")
       "countColumnDuplicateValues"   -> countColumnDuplicateValues <$> (mkColMay <$> (o .: "column") <*> o .:? "columnExport")
       _                              -> fail ("unknown filter: " ++ filterName)
instance ToJSON Filter where
    toJSON _ = "" --TODO: implement serialization as well?  Need new data type to tag how to serialize if so

_simpleJson :: Maybe ImportDefinition
_simpleJson = decode "{\"columns\":[{\"importName\":\"a\"}],\"filters\":[{\"filter\":\"findAndReplace\", \"oldString\":\"a\", \"newString\":\"a\", \"column\":\"a\"}]}"
