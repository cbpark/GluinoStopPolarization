{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Calculation.Variables
import           Interface.IOHelper              (removeIfExists)

import           HEP.Data.LHEF
import           HEP.Data.LHEF.Parser

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.State
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as C
import qualified Data.Map                        as Map
import           Options.Applicative
import           System.IO                       (Handle, IOMode (..), withFile)

data Args = Args { input :: String, output :: String }

cmdoptions :: Parser Args
cmdoptions = Args <$> strOption ( long "input"
                               <> metavar "LHEF"
                               <> help "Input LHEF file" ) <*>
                      strOption ( long "output"
                               <> metavar "OUTPUT"
                               <> help "Output file to save the result" )

calcAndSave :: Args -> IO ()
calcAndSave (Args infile outfile) = infile `parseCalcSave` outfile

parseCalcSave :: FilePath -> FilePath -> IO ()
parseCalcSave infile outfile = do
  removeIfExists outfile
  evstr <- C.readFile infile
  ntot <- withFile outfile WriteMode $ \hdl -> do
                                 writeHeader hdl
                                 execStateT ((parseCalcSave' . stripLHEF)
                                             evstr hdl) 0
  C.putStrLn . C.pack $ "Total number of events parsed = " ++ show (ntot - 1)
    where
      writeHeader :: Handle -> IO ()
      writeHeader h = C.hPutStrLn h $
                      "# " `C.append` C.intercalate ", " (Map.keys var)

      parseCalcSave' :: C.ByteString -> Handle -> StateT Integer IO ()
      parseCalcSave' s h = do
            modify (+1)
            case parse parseEvent s of
              Fail r _ _               -> liftIO $ C.putStr r
              Done evRemained evParsed -> do
                                      printResult (snd evParsed) h
                                      parseCalcSave' evRemained h

printResult :: ParticleMap -> Handle -> StateT Integer IO ()
printResult pm hdl = do
  neve <- get
  let result = sequence (Map.elems var) pm
  liftIO $ B.hPutStrLn hdl $
         B.pack (show neve ++ ", ") `B.append` B.intercalate ", " result

main :: IO ()
main =
  execParser opts >>= calcAndSave
      where opts = info (helper <*> cmdoptions)
                   ( fullDesc
                  <> progDesc ( "Calculate collider variables for top quarks" ++
                                " in the gluino decays" )
                  <> header "GluinoStop_calc - calculate collider variables" )
