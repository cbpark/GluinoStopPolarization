{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Interface.IOHelper              (removeIfExists)
import           Parton.Variables                (var, varallcomb)

import           HEP.Data.LHEF
import           HEP.Data.LHEF.Parser

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.State
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as C
import qualified Data.Map                        as Map
import           Database.HDBC
import           Database.HDBC.Sqlite3           (Connection, connectSqlite3)
import           Options.Applicative

data Args = Args { input :: String, output :: String }

cmdoptions :: Parser Args
cmdoptions = Args <$> argument str ( metavar "LHEF"
                                  <> help "Input LHEF file") <*>
                      strOption ( long "output"
                               <> short 'o'
                               <> metavar "DB"
                               <> help "Output DB file to save the result" )

calcAndSave :: Args -> IO ()
calcAndSave (Args infile outfile) = infile `parseCalcSave` outfile

parseCalcSave :: FilePath -> FilePath -> IO ()
parseCalcSave infile outfile = do
  removeIfExists outfile
  conn <- prepareDB outfile
  evstr <- C.readFile infile
  ntot <- execStateT ((parseCalcSave' . stripLHEF) evstr conn) 0
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
                      (Map.keys var) ++ ");") []
  run conn ("CREATE TABLE varallcomb (neve INTEGER PRIMARY KEY" ++
            concatMap (\v -> ", " ++ C.unpack v ++ " TEXT")
                      (Map.keys varallcomb) ++ ");") []
  commit conn
  return conn

insertResult :: ParticleMap -> Connection -> StateT Integer IO ()
insertResult pm conn = do
  neve <- get
  let !varResult = toSql neve : map toSql (sequence (Map.elems var) pm)
      !varcomballResult = toSql neve : map (toSql . B.intercalate ", ")
                          (sequence (Map.elems varallcomb) pm)
  liftIO $ do
    run conn ("INSERT INTO var (neve, " ++
              insertAll var ++ "?)") varResult
    run conn ("INSERT INTO varallcomb (neve, " ++
              insertAll varallcomb ++ "?)") varcomballResult
    commit conn
        where insertAll vm =
                  C.unpack (C.intercalate ", " (Map.keys vm)) ++
                  ") VALUES (" ++
                  concat (replicate (length (Map.keys vm)) "?, ")

main :: IO ()
main = execParser opts >>= calcAndSave
    where opts = info (helper <*> cmdoptions)
                 ( fullDesc
                <> progDesc ( "Calculate collider variables for top quarks" ++
                              " in the gluino decays" )
                <> header "GluinoStop_calc - calculate collider variables" )
