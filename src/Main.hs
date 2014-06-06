{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Calculation.Variables

import           HEP.Data.LHEF.Parser

import           Control.Monad                   (when)
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as C
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure)

parseAndCalc :: C.ByteString -> IO ()
parseAndCalc str = case parse parseEvent str of
                     Fail r _ _               -> C.putStr r
                     Done evRemained evParsed ->
                         do let parmap = snd evParsed
                            B.putStrLn $ eRatioBLTruePair parmap
                            -- B.putStrLn $ B.intercalate "  " (eRatioBLAllPair parmap)
                            parseAndCalc evRemained

main :: IO ()
main = do
  args <- getArgs

  when (length args /= 1) $ do
         putStrLn "Supply the input file name."
         exitFailure

  evstr <- C.readFile (head args)
  (parseAndCalc . stripLHEF) evstr
