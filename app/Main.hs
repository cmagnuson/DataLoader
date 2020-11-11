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
import           Text.ParserCombinators.Parsec       (parse)
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

numberColumn :: Column
numberColumn = Column "RaceNumber" "Bib"
relayColumn :: Column
relayColumn = Column "Team Name" "RELAY_TEAM"
eventColumn :: Column
eventColumn = Column "Race Name" "ASSIGNED_EVENT"

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
                                return (file, T.pack file1', wwImport)
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

knoxvilleImport :: ImportDefinition
knoxvilleImport = ImportDefinition [
              (Column "Bib" "No."),
              (mkCol "First Name"),
              (mkCol "Last Name"),
              (Column "E-mail Address" "Email"),
              (Column "Middle Name" "MI"),
              (Column "Gender" "Sex"),
              (mkCol "Age"),
              (Column "Date of Birth" "DOB"),
              (Column "Street Address" "ADDRESS"),
              (mkCol "Country"),
              (mkCol "City"),
              (Column "Zip Code" "Zip"),
              (Column "Race Group" "Team"),
              (Column "Corporate Team" "Corp Team"),
              (Column "Event" "Race"),
              (Column "Race Group - Type" "Group Type"),
              (mkCol "Emergency Contact Name"),
              (mkCol "Emergency Contact Phone"),
              (mkCol "Registration ID")
              ] [
                deleteFilter "" (Column "Bib" "No.")
                , stripSpecialCharsFilter
              -- , fileSplitOnColumnEqualsFilter (Column "Event" "Race") "S" "Skate"
              , fileSplitOnColumnFilter (Column "Bib" "No.")
              , countColumnDuplicateValues (Column "Bib" "No.")
              , countColumnUniqueValues (Column "Event" "Race")
              , countColumnUniqueValues (Column "Corporate Team" "Corp Team")
              , countColumnUniqueValues (Column "Race Group" "Team")
              , countColumnUniqueValues (Column "Gender" "Sex")
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

_reindeerRun :: ImportDefinition
_reindeerRun = ImportDefinition [
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
              Column "Shoutout" "Notes",
              Column "Race" "Mobility"
              ] [
              countColumnDuplicateValues (Column "BibNumber" "no.")
              , countColumnDuplicateValues (Column "Runnerid" "regid")
              , stripSpecialCharsFilter
              , countColumnDuplicateValues (mkCol "Tagid")
              , countColumnUniqueValues (mkCol "Country")
              , countColumnUniqueValues (Column "FalmouthResident" "FALMOUTH")
              , countColumnUniqueValues (Column "Gender" "sex")
              , countColumnUniqueValues (Column "Race" "Mobility")
              , findAndReplaceExactMatchFilter "FRR" "" (Column "Race" "Mobility")
              , findAndReplaceExactMatchFilter "FRRPRW" "" (Column "Race" "Mobility")
              , findAndReplaceExactMatchFilter "FRREW" "" (Column "Race" "Mobility")
              , findAndReplaceExactMatchFilter "FRRDUO" "" (Column "Race" "Mobility")
              , findAndReplaceExactMatchFilter "FRRAWD" "X" (Column "Race" "Mobility")
              ]

wwImport :: ImportDefinition
wwImport = ImportDefinition [
  mkCol "age",
  mkCol "FIRST NAME",
  mkCol "LAST NAME",
  mkCol "BIRTHDATE",
  mkCol "SEX",
  mkCol "ADDRESS",
  mkCol "ZIP",
  mkCol "CITY",
  mkCol "STATE",
  mkCol "COUNTRY",
  mkCol "EMAIL",
  mkCol "PHONE",
  mkCol "REGID",
  mkCol "TRANSID",
  mkCol "ASSIGNED_EVENT",
  mkCol "SHIRT",
  mkCol "WHEEL",
  mkCol "No.",
  mkCol "KITUPGRADE",
  mkCol "KITNUMBER"
  ][
    stripSpecialCharsFilter
  , countColumnDuplicateValues (mkCol "No.")
  , countColumnDuplicateValues (mkCol "TRANSID")
  , countColumnDuplicateValues (mkCol "REGID")
  , countColumnUniqueValues (mkCol "ASSIGNED_EVENT")
  , countColumnUniqueValues (mkCol "COUNTRY")
  , countColumnUniqueValues (mkCol "SHIRT")
  , countColumnUniqueValues (mkCol "WHEEL")
  , countColumnUniqueValues (mkCol "KITUPGRADE")
  , countColumnUniqueValues (mkCol "KITNUMBER")
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
            mkCol "Zip",
            mkCol "Duo",
            Column "DOB" "Birthdate"
            ] [
              findAndReplaceFilter "10 Mile" "M" (Column "MasterEvent" "Assigned_Event"),
              findAndReplaceFilter "10K" "K" (Column "MasterEvent" "Assigned_Event"),
              findAndReplaceFilter "5K" "5" (Column "MasterEvent" "Assigned_Event"),
              findAndReplaceFilter "One Mile" "1" (Column "MasterEvent" "Assigned_Event"),
              findAndReplaceFilter "TRUE" "X" (mkCol "Duo"),
              findAndReplaceFilter "FALSE" "" (mkCol "Duo"),
              countColumnDuplicateValues   (Column "Race Number" "No.")
            , countColumnDuplicateValues (Column "Confirm Code" "REGID")
            , stripSpecialCharsFilter
            , countColumnUniqueValues (Column "MasterEvent" "Assigned_Event")
            , countColumnUniqueValues (Column "Gender" "Sex")
            , countColumnUniqueValues (mkCol "Duo")
            ]

_rwbImport :: ImportDefinition
_rwbImport = ImportDefinition [
          numberColumn,
          (Column "First Name" "First Name"),
          (Column "Last Name" "Last Name"),
          (Column "Gender" "Sex"),
          (Column "Confirm Code" "REGID"),
          (Column "Challenge Series Number" "CHALLENGE_CODE"),
          relayColumn,
          eventColumn,
          (Column "Address 1" "Address"),
          (Column "City" "City"),
          (Column "State" "State"),
          (Column "Zip" "Zip"),
          (Column "Event Age" "Age"),
          (Column "DOB" "Birthdate"),
          (Column "Email Address" "Email"),
          (Column "Registered" "DATE_REGISTERED")
          ] [
          --(findAndReplaceFilter "-1" "" numberColumn),
          --(findAndReplaceFilter "-2" "" numberColumn),
          (findAndReplaceFilter " (captain)" "" relayColumn),
          (findAndReplaceFilter "Relay" "H" eventColumn),
        --  (fileSplitOnColumnFilter numberColumn),
          countColumnDuplicateValues   numberColumn
        , countColumnDuplicateValues (Column "Confirm Code" "REGID")
        , stripSpecialCharsFilter
        , countColumnUniqueValues eventColumn
        , countColumnUniqueValues (Column "Gender" "Sex")
          ]
