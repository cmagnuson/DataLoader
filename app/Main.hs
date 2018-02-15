{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           Control.Monad.Writer
import           Data.Aeson
import qualified Data.ByteString                     as B
import           Data.ByteString.Lazy                (fromStrict)
import           Data.ByteString.Lazy.Char8          as C8 (pack)
import           Data.Maybe
import qualified Data.Text                           as T
import           Data.Text.Encoding                  (decodeUtf8With,
                                                      encodeUtf8)
import           Data.Text.Encoding.Error
import           Data.Tree
import           Files
import           Filters
import           Json
import           Summary
import           System.Environment
import           Text.CSV
import           Text.ParserCombinators.Parsec       hiding (Column)
import           Text.ParserCombinators.Parsec.Error
import qualified Text.Show.Pretty                    as PP
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
  args <- getArgs
  (file, filename, importDefinition) <- processArgs args
  let (files, logs) = runImport importDefinition (parse csv "Error Parsing" $ T.unpack file) filename
  mapM_ (saveFile files) [0 .. length files - 1]
  putStrLn logs

processArgs :: [String] -> IO (T.Text, T.Text, ImportDefinition)
processArgs [file1']         = do
                                file <- handleFile file1'
                                return (file, T.pack file1', reindeerRun)
processArgs [file1', file2'] = do
                                file <- handleFile file1'
                                json <- handleJson file2'
                                return (file, T.pack file1', json)
processArgs _                = fail "Invalid number of arguments"

handleFile :: String -> IO T.Text
handleFile filename = do
                        bs <- B.readFile filename
                        return  (decodeUtf8With lenientDecode $ bs)

handleJson :: String -> IO ImportDefinition
handleJson filename = do
                        str <- handleFile filename
                        return (fromJust $ decode $ fromStrict $ encodeUtf8 str)


saveFile :: [(T.Text, ImportFile)] -> Int -> IO()
saveFile files idx = writeFile (T.unpack (fst (files !! idx) <> ".csv")) (fileToString (snd $ files !! idx))

runImport :: ImportDefinition ->  Either ParseError CSV -> T.Text -> ([(T.Text, ImportFile)], String)
runImport importDefinition (Right csv1)  fileSuffix =
  let
   (files, logs) = runWriter (processImport importDefinition (csvToFile csv1) fileSuffix)
   in
    (files, PP.ppShow logs)
runImport _ (Left parseError) _                         = error ("Error parsing: " <> concatMap messageString (errorMessages parseError))

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

_mnHalfImport :: ImportDefinition
_mnHalfImport = ImportDefinition [
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

_mplsHalloweenImport :: ImportDefinition
_mplsHalloweenImport = ImportDefinition [
              Column "Bib Numbers" "no.",
              (mkCol "First Name"),
              (mkCol "Last Name"),
              (mkCol "Sex"),
              (mkCol "Age"),
              Column "Date of Birth" "Birthdate",
              (mkCol "Address"),
              Column "ZIP/Postal Code" "Zip",
              (mkCol "City"),
              (mkCol "State"),
              (mkCol "Email"),
              Column "Phone Number" "Phone",
              Column "Participant ID" "regid",
              Column "Corporate Team Challenge - what company do you work for?" "corp_team",
              Column "Select your Run Club here:" "run_club",
              Column "Sub Event" "ASSIGNED_EVENT"
              ] [
                deleteFilter "Mail My Bib Option" (Column "Sub Event" "ASSIGNED_EVENT")
              , deleteFilter "" (Column "Bib Numbers" "no.")
              , countColumnDuplicateValues (Column "Bib Numbers" "no.")
              , countColumnUniqueValues (Column "Sub Event" "ASSIGNED_EVENT")
              , countColumnUniqueValues (Column "Corporate Team Challenge - what company do you work for?" "corp_team")
              , countColumnUniqueValues (Column "Select your Run Club here:" "run_club")
              , countColumnUniqueValues (mkCol "Sex")
              ]

reindeerRun :: ImportDefinition
reindeerRun = ImportDefinition [
              Column "Bib Numbers" "no.",
              (mkCol "First Name"),
              (mkCol "Last Name"),
              (mkCol "Sex"),
              (mkCol "Age"),
              Column "Date of Birth" "Birthdate",
              (mkCol "Address"),
              Column "ZIP/Postal Code" "Zip",
              (mkCol "City"),
              (mkCol "State"),
              (mkCol "Email"),
              Column "Phone Number" "Phone",
              Column "Participant ID" "regid",
              Column "Corporate Team Challenge - what company do you work for?" "corp_team",
              Column " Select your Run Club here:" "run_club",
              Column "Sub Event" "ASSIGNED_EVENT"
              ] [
                deleteFilter "Mail My Bib Option" (Column "Sub Event" "ASSIGNED_EVENT")
              , deleteFilter "" (Column "Bib Numbers" "no.")
              , countColumnDuplicateValues (Column "Bib Numbers" "no.")
              , countColumnUniqueValues (Column "Sub Event" "ASSIGNED_EVENT")
              , countColumnUniqueValues (Column "Corporate Team Challenge - what company do you work for?" "corp_team")
              , countColumnUniqueValues (Column " Select your Run Club here:" "run_club")
              , countColumnUniqueValues (mkCol "Sex")
              , fileSplitOnColumnEqualsFilter (Column "Sub Event" "ASSIGNED_EVENT") "5K Run - Chip Timed" "5K"
              , fileSplitOnColumnEqualsFilter (Column "Sub Event" "ASSIGNED_EVENT") "10K Run - Chip Timed" "10K"
              , fileSplitOnColumnEqualsFilter (Column "Sub Event" "ASSIGNED_EVENT") "Yukon Challenge (10K + 5K)" "Yukon"
              ]


_falImport :: ImportDefinition
_falImport = ImportDefinition [
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

_wrtcImport :: ImportDefinition
_wrtcImport = ImportDefinition [
            Column "MasterEvent" "Assigned_Event",
            Column "Confirm Code" "REGID",
            Column "Race Number" "No.",
            mkCol "Last Name",
            mkCol "First Name",
            Column "Gender" "Sex",
            Column "Event Age" "Age",
            Column "Email Address" "Email",
            Column "Address1" "Address",
            mkCol "City",
            mkCol "State",
            mkCol "Zip"
            ] [
              findAndReplaceFilter "10 Mile" "M" (Column "MasterEvent" "Assigned_Event"),
              findAndReplaceFilter "10K" "K" (Column "MasterEvent" "Assigned_Event"),
              findAndReplaceFilter "5K" "5" (Column "MasterEvent" "Assigned_Event"),
              findAndReplaceFilter "One Mile" "1" (Column "MasterEvent" "Assigned_Event"),
              countColumnDuplicateValues   (Column "Race Number" "No.")
            , countColumnDuplicateValues (Column "Confirm Code" "REGID")
            , stripSpecialCharsFilter
            , countColumnUniqueValues (Column "MasterEvent" "Assigned_Event")
            , countColumnUniqueValues (Column "Gender" "Sex")
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
