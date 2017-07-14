{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Writer
import qualified Data.Text                     as T
import           Files
import           Filters
import           System.Environment
import           Text.CSV
import           Text.ParserCombinators.Parsec hiding (Column)
import qualified Text.Show.Pretty              as PP
import           Types

processImport :: ImportDefinition -> File -> Writer[T.Text] [File]
processImport _ [] = return []
processImport (ImportDefinition colmns fltrs) (header : rows) =
  let
    (rowss, logs) = runWriter (filterRows [toRows rows header colmns] fltrs)
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

main :: IO ()
main = do
  [file1'] <- getArgs
  file1 <- parseCSVFromFile file1'
  let (files, logs) = runImport mnHalfImport file1;
  mapM_ (saveFile file1' files) [0 .. length files - 1]
  putStrLn logs

saveFile :: String -> [File] -> Int -> IO()
saveFile path files idx = writeFile (path <> "_processed_" <> show (idx+1) <> ".csv") (fileToString (files !! idx))

runImport :: ImportDefinition -> Either ParseError CSV -> ([File], String)
runImport importDefinition (Right csv1) =
  let
   (files, logs) = runWriter (processImport importDefinition $ csvToFile csv1)
   in
    (files, PP.ppShow logs)
runImport _ _                          = error "FIXME: error parsing"

csvToFile :: CSV -> File
csvToFile = fmap $ fmap T.pack

_testCsv :: File
_testCsv = [["name","age"],["jack","21"],["james","25"]] :: File

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
              (mkCol "sex"),
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
                deleteFilter "" (mkCol "ASSIGNED_EVENT")
              , deleteFilter "" (mkCol "no.")
              , fileSplitOnColumnFilter (mkCol "no.")
              -- TODO: split also those with S ASSIGNED_EVENT (skate) into seprate file then those with anything else
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
