{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Writer
import           Data.ByteString               (ByteString, readFile)
import qualified Data.Text                     as T
import           Data.Text.Encoding
import           Data.Text.Encoding.Error
import           Data.Tree
import           Files
import           Filters
import           Summary
import           System.Environment
import           Text.CSV
import           Text.ParserCombinators.Parsec hiding (Column)
import qualified Text.Show.Pretty              as PP
import           Types

processImport :: ImportDefinition -> ImportFile -> T.Text -> Writer[[T.Text]] [(T.Text, ImportFile)]
processImport _ [] _ = return []
processImport (ImportDefinition colmns fltrs) (header : rows) fileSuffix =
  let
    (rowss, logs) = runWriter (filterRows  Node {rootLabel = (fileSuffix, toRows rows header colmns), subForest=[]} fltrs)
    in do
      tell logs
      return (fromRows rowss)
--  fromRows (filterRows [(toRows rows header colmns)] fltrs)

_numberColumn :: Column
_numberColumn = Column "RaceNumber" "No."
_relayColumn :: Column
_relayColumn = Column "Display/TeamName" "RELAY_TEAM"
_eventColumn :: Column
_eventColumn = Column "Event Name" "ASSIGNED_EVENT"

--TODO: read file as bytestring, then try and convert to UTF8 -> decodeUtf8With lenientDecode bs, then try and parse as csv
main :: IO ()
main = do
  [file1'] <- getArgs
  bs <- Data.ByteString.readFile file1' :: IO ByteString
  --file1 <- (parse csv "a" $ T.unpack $ decodeUtf8With lenientDecode bs)
  --file1 <- parseCSVFromFile file1'
  let (files, logs) = runImport falImport (parse csv "Error Parsing" $ T.unpack $ decodeUtf8With lenientDecode bs) (T.pack file1');
  mapM_ (saveFile files) [0 .. length files - 1]
  putStrLn logs

saveFile :: [(T.Text, ImportFile)] -> Int -> IO()
saveFile files idx = writeFile (T.unpack (fst (files !! idx) <> ".csv")) (fileToString (snd $ files !! idx))

runImport :: ImportDefinition ->  Either ParseError CSV -> T.Text -> ([(T.Text, ImportFile)], String)
runImport importDefinition (Right csv1)  fileSuffix =
  let
   (files, logs) = runWriter (processImport importDefinition (csvToFile csv1) fileSuffix)
   in
    (files, PP.ppShow logs)
runImport _ _ _                         = error "FIXME: error parsing"

csvToFile :: CSV -> ImportFile
csvToFile = fmap $ fmap T.pack

_testCsv :: ImportFile
_testCsv = [["name","age"],["jack","21"],["james","25"]] :: ImportFile

_testImport :: ImportDefinition
_testImport = ImportDefinition [
    (Column "name" "name"),
    (Column "age" "age")
  ] [
    (deleteFilter "25" (Column "age" "age"))
      -- , (findAndReplace "21" "x" (Column "age" "age"))
  ]

mnHalfImport :: ImportDefinition
mnHalfImport = ImportDefinition [
              (mkCol "no."),
              (mkCol "First Name"),
              (mkCol "Last Name"),
              (mkCol "Sex"),
              (mkCol "age"),
              (mkCol "birthdate"),
              (mkCol "ADDRESS"),
              (mkCol "ZIP"),
              (mkCol "CITY"),
              (mkCol "STATE"),
              (mkCol "EMAIL"),
              (mkCol "phone"),
              (mkCol "regid"),
              (mkCol "corp_team"),
              (mkCol "run_club"),
              (mkCol "relay_team"),
              (mkCol "ASSIGNED_EVENT")
              ] [
--                deleteFilter "" (mkCol "ASSIGNED_EVENT")
                deleteFilter "C" (mkCol "ASSIGNED_EVENT")
              , deleteFilter "" (mkCol "no.")
              , fileSplitOnColumnEqualsFilter (mkCol "ASSIGNED_EVENT") "S" "Skate"
              , fileSplitOnColumnFilter (mkCol "no.")
              , countColumnDuplicateValues (mkCol "no.")
              , countColumnUniqueValues (mkCol "ASSIGNED_EVENT")
              , countColumnUniqueValues (mkCol "Sex")
              -- TODO: neater file handling - better nesting of splits and naming conventions
              ]

falImport :: ImportDefinition
falImport = ImportDefinition [
              Column "Runnerid" "regid",
              Column "BibNumber" "no.",
              Column "FullBibNumber" "bib",
              mkCol "Tagid",
              Column "FirstName" "first name",
              Column "LastName" "last name",
              Column "Gender" "sex",
              Column "DateOfBirth" "birthdate",
              Column "RaceDayAge" "age",
              mkCol "City",
              mkCol "State",
              mkCol "Country",
              Column "Citizenship" "country_ctz",
              Column "Address1" "address",
              mkCol "Zip",
              mkCol "Email",
              Column "FalmouthResident" "FALMOUTH",
              Column "Shoutout" "Notes"
              ] [
              countColumnDuplicateValues (Column "BibNumber" "no.")
              , countColumnDuplicateValues (Column "Runnerid" "regid")
              -- , findAndReplaceFilter "\"" "" (Column "FirstName" "first name") -- TODO: replace on all columns special charecters
              -- , findAndReplaceFilter "\"" "" (Column "LastName" "last name")
              , stripSpecialCharsFilter
              , countColumnDuplicateValues (mkCol "Tagid")
              , countColumnUniqueValues (mkCol "Country")
              , countColumnUniqueValues (Column "FalmouthResident" "FALMOUTH")
              , countColumnUniqueValues (Column "Gender" "sex")
              ]

_rwbImport :: ImportDefinition
_rwbImport = ImportDefinition [
          _numberColumn,
          (Column "FirstName" "First Name"),
          (Column "LastName" "Last Name"),
          (Column "Gender" "Sex"),
          (Column "Confirmation Code" "REGID"),
          (Column "Series Code" "CHALLENGE_CODE"),
          _relayColumn,
          _eventColumn,
          (Column "Address" "Address"),
          (Column "City" "City"),
          (Column "State" "State"),
          (Column "Zip" "Zip"),
          (Column "Age" "Age"),
          (Column "Birthdate" "Birthdate"),
          (Column "Email" "Email"),
          (Column "DateEntered" "DATE_REGISTERED")
          ] [
    --      (sort
    --      (findAndReplace "-1" "" numberColumn),
    --      (findAndReplace "-2" "" numberColumn),
    --      (findAndReplace " (captain)" "" relayColumn),
    --      (findAndReplace "Relay" "H" eventColumn),
    --      (split numberColumn)
          ]
