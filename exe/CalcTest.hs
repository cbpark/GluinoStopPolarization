{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Interface.IOHelper              (removeIfExists)
import           Jet.Selection

import           HEP.Data.LHEF
import           HEP.Data.LHEF.Parser

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
-- import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as C
import           Options.Applicative
import           System.IO

data Args = Args { input :: String, output :: String }

data EventType = Parton | Jet deriving Eq

cmdoptions :: Parser Args
cmdoptions = Args <$> argument str ( metavar "LHEF"
                                  <> help "Input LHEF file") <*>
                      strOption ( long "output"
                               <> short 'o'
                               <> metavar "OUTPUT"
                               <> help "Output file to save the result" )

calcAndSave :: Args -> IO ()
calcAndSave (Args infile outfile) = parseCalcSave infile outfile

parseCalcSave :: FilePath -> FilePath -> IO ()
parseCalcSave infile outfile = do
  removeIfExists outfile
  evstr <- C.readFile infile

  ntot <- execStateT ((parseCalcSave' . stripLHEF) evstr stdout) 0

  C.putStrLn . C.pack $ "Total number of events parsed = " ++ show (ntot - 1)
    where
      parseCalcSave' :: C.ByteString -> Handle -> StateT Integer IO ()
      parseCalcSave' s h = do
        modify (+1)
        case parse parseEvent s of
          Fail r _ _               -> liftIO $ C.putStr r
          Done evRemained evParsed -> do
                                  printResult (snd evParsed) h
                                  parseCalcSave' evRemained h

printResult :: ParticleMap -> Handle -> StateT Integer IO ()
printResult pm _ = do
  let result = runReader finalObjs pm
  liftIO $ print result

main :: IO ()
main =
  execParser opts >>= calcAndSave
      where opts = info (helper <*> cmdoptions)
                   ( fullDesc
                  <> progDesc ( "Calculate collider variables for top quarks" ++
                                " in the gluino decays" )
                  <> header "GluinoStop_calctest - calculate collider variables" )
