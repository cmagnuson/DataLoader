{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Writer
import qualified Data.Text                     as T
import           Files
import           Filters
import           Lucid
import           System.Environment
import           Text.CSV
import           Text.ParserCombinators.Parsec hiding (Column)
import qualified Text.Show.Pretty              as PP
import           Types

processImport :: ImportDefinition -> File -> Writer[String] [File]
processImport _ [] = return []
processImport (ImportDefinition colmns fltrs) (header : rows) =
  let
    (rowss, logs) = runWriter (filterRows [toRows rows header colmns] fltrs)
    in do
      tell logs
      return (fromRows rowss)
--  fromRows (filterRows [(toRows rows header colmns)] fltrs)

numberColumn :: Column
numberColumn = Column "RaceNumber" "No."
relayColumn :: Column
relayColumn = Column "Display/TeamName" "RELAY_TEAM"
eventColumn :: Column
eventColumn = Column "Event Name" "ASSIGNED_EVENT"

main :: IO ()
main = do
  [file1'] <- getArgs
  file1 <- parseCSVFromFile file1'
  putStrLn $ runImport llsImport file1

runImport :: ImportDefinition -> Either ParseError CSV -> String
runImport importDefinition (Right csv1) =
  let
   (files, logs) = runWriter (processImport importDefinition $ csvToFile csv1)
   in
    PP.ppShow files ++ PP.ppShow logs
runImport _ _                          = error "FIXME: error parsing"

csvToFile :: CSV -> File
csvToFile = fmap $ fmap T.pack

testCsv :: File
testCsv = [["name","age"],["jack","21"],["james","25"]] :: File

testImport :: ImportDefinition
testImport = ImportDefinition [(Column "name" "name"), (Column "age" "age")] [(delete "25" (Column "age" "age")), (findAndReplace "21" "x" (Column "age" "age"))]

llsImport :: ImportDefinition
llsImport = ImportDefinition [
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
                deleteIfContains "K" (mkCol "ASSIGNED_EVENT")
              , deleteIfContains "N" (mkCol "ASSIGNED_EVENT")
              , delete "" (mkCol "no.")
              , fileSplitOnColumn (mkCol "no.")
              ]

rwbImport :: ImportDefinition
rwbImport = ImportDefinition [
          numberColumn,
          (Column "FirstName" "First Name"),
          (Column "LastName" "Last Name"),
          (Column "Gender" "Sex"),
          (Column "Confirmation Code" "REGID"),
          (Column "Series Code" "CHALLENGE_CODE"),
          relayColumn,
          eventColumn,
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
