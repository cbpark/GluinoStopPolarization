{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Calculation.Variables

import           HEP.Data.LHEF
import           HEP.Data.LHEF.Parser

import           Control.Monad                   (when)
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as C
import           Database.HDBC
import           Database.HDBC.Sqlite3           (connectSqlite3)
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure)

parseAndCalc :: C.ByteString -> IO ()
parseAndCalc str =
    case parse parseEvent str of
      Fail r _ _               -> C.putStr r
      Done evRemained evParsed ->
          do let parmap = snd evParsed
             printResult parmap
             parseAndCalc evRemained

printResult :: ParticleMap -> IO ()
printResult pm = do
  let result = sequence [ eRatioTrue
                        , eRatioByM
                        , eRatioByPT
                        , eRatioByTheta
                        , mBLTrue
                        , missingET
                        ] pm
  B.putStrLn (B.intercalate "   " result)

main :: IO ()
main = do
  args <- getArgs

  when (length args /= 1) $ do
         putStrLn "Supply the input file name."
         exitFailure

  evstr <- C.readFile (head args)
  (parseAndCalc . stripLHEF) evstr

  conn <- connectSqlite3 "test.db"
  run conn ("CREATE TABLE var (id INTEGER PRIMARY KEY AUTOINCREMENT" ++
            ", eRatioTrue REAL" ++
            ", eRatioByM REAL" ++
            ", eRatioByPT REAL" ++
            ", eRatioByTheta REAL" ++
            ")"
           ) []
  run conn "INSERT INTO var (eRatioTrue, eRatioByM, eRatioByPT, eRatioByTheta) VALUES (?, ?, ?, ?)"
          [ toSql (1 :: Double)
          , toSql (2 :: Double)
          , toSql (3 :: Double)
          , toSql (4 :: Double)]
  commit conn
  disconnect conn
