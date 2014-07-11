{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Interface.IOHelper              (removeIfExists)
import           Jet.Variables                   (calcVar)

import           HEP.Data.LHEF
import           HEP.Data.LHEF.Parser

import           Control.Monad                   (when)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Reader      (runReader)
import           Control.Monad.Trans.State
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import qualified Data.ByteString.Lazy.Char8      as C
import qualified Data.Map                        as Map
import           Database.HDBC
import           Database.HDBC.Sqlite3           (Connection, connectSqlite3)
import           Options.Applicative
import           System.FilePath                 (takeBaseName)

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
  conn <- connectSqlite3 outfile
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
                                       insertResult (snd evParsed) c infile
                                       parseCalcSave' evRemained c

insertResult :: ParticleMap -> Connection -> FilePath -> StateT Integer IO ()
insertResult pm conn infile = do
  let !result = runReader calcVar pm
  neve <- get
  when (neve == 1) $ liftIO (prepareDB result)
  liftIO $ do
    run conn ("INSERT INTO var (neve, " ++
              C.unpack (C.intercalate ", " (Map.keys result)) ++
              ") VALUES (" ++ concat (replicate (Map.size result) "?, ") ++ "?)") $
           toSql (takeBaseName infile ++ "-" ++ show neve) :
           map toSql (Map.elems result)
    commit conn
      where prepareDB r = do run conn ("CREATE TABLE var (neve " ++
                                       "TEXT PRIMARY KEY" ++
                                       concatMap (\v -> ", " ++ C.unpack v ++ " REAL")
                                       (Map.keys r) ++ ");") []
                             commit conn

main :: IO ()
main = execParser opts >>= calcAndSave
    where opts = info (helper <*> cmdoptions)
                 ( fullDesc
                <> progDesc ( "Calculate collider variables for top quarks" ++
                              " in the gluino decays" )
                <> header "GluinoStop_calcvar - calculate collider variables" )
