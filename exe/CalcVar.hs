{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Calculation.Variables

import           HEP.Data.LHEF
import           HEP.Data.LHEF.Parser

import           Control.Exception               (catch, throwIO)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.State       (StateT (..), get, modify)
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import qualified Data.ByteString.Lazy.Char8      as C
import qualified Data.Map                        as Map
import           Database.HDBC
import           Database.HDBC.Sqlite3           (Connection, connectSqlite3)
import           Options.Applicative
import           System.Directory                (removeFile)
import           System.IO.Error                 (isDoesNotExistError)

data Args = Args { input :: String, output :: String }

cmdoptions :: Parser Args
cmdoptions = Args <$> strOption ( long "input"
                               <> metavar "LHEF"
                               <> help "Input LHEF file" ) <*>
                      strOption ( long "output"
                               <> metavar "DB"
                               <> help "Output DB file to save the result" )

calcAndSave :: Args -> IO ()
calcAndSave (Args infile outfile) = infile `parseCalcSave` outfile

parseCalcSave :: FilePath -> FilePath -> IO ()
parseCalcSave infile outfile = do
  removeIfExists outfile
  conn <- prepareDB outfile
  evstr <- C.readFile infile
  (_, ntot) <- runStateT ((parseCalcSave' . stripLHEF) evstr conn) 0
  disconnect conn
  C.putStrLn . C.pack $ "-- Total number of events parsed = " ++ show (ntot - 1)
  C.putStrLn . C.pack $ "-- " ++ outfile ++ " has been created."
      where
        parseCalcSave' :: C.ByteString -> Connection -> StateT Integer IO ()
        parseCalcSave' s c = do
          modify (+1)
          case parse parseEvent s of Fail r _ _ -> liftIO $ C.putStr r
                                     Done evRemained evParsed -> do
                                       insertResult (snd evParsed) c
                                       parseCalcSave' evRemained c

prepareDB :: FilePath -> IO Connection
prepareDB outfile = do
  conn <- connectSqlite3 outfile
  run conn ("CREATE TABLE var (neve INTEGER PRIMARY KEY" ++
            concatMap (\v -> ", " ++ C.unpack v ++ " REAL")
                      (Map.keys variables) ++ ");") []
  commit conn
  return conn

insertResult :: ParticleMap -> Connection -> StateT Integer IO ()
insertResult pm conn = do
  neve <- get
  let result = sequence (Map.elems variables) pm
      resultval = toSql neve : map toSql result
  liftIO $ do
    run conn ("INSERT INTO var (neve, " ++
              C.unpack (C.intercalate ", " (Map.keys variables)) ++
              ") VALUES (" ++ concat (replicate (length result) "?, ") ++
              "?)") resultval
    commit conn

removeIfExists :: FilePath -> IO ()
removeIfExists f = removeFile f `catch` handleExists
  where handleExists e | isDoesNotExistError e = return ()
                       | otherwise             = throwIO e

main :: IO ()
main =
    execParser opts >>= calcAndSave
        where opts = info (helper <*> cmdoptions)
                   ( fullDesc
                  <> progDesc ( "Calculate collider variables for top quarks" ++
                                " in the gluino decays" )
                  <> header "GluinoStop_calc - calculate collider variables" )
