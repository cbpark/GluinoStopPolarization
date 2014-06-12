{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Calculation.Variables

import           HEP.Data.LHEF
import           HEP.Data.LHEF.Parser

import           Control.Exception               (catch, throwIO)
import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.State       (StateT (..), get, modify)
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as C
import           Options.Applicative
import           System.Directory                (removeFile)
import           System.IO                       (Handle, IOMode (..), withFile)
import           System.IO.Error                 (isDoesNotExistError)

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
  (_, ntot) <- withFile outfile WriteMode $ \hdl -> do
                                 writeHeader hdl
                                 runStateT ((parseCalcSave' . stripLHEF) evstr hdl) 0
  C.putStrLn . C.pack $ "Total number of events parsed = " ++ show (ntot - 1)
    where
      writeHeader :: Handle -> IO ()
      writeHeader h = C.hPutStrLn h $
                      C.pack "# " `C.append` C.intercalate ", " [ "nEvent"
                                                                , "eRTrue"
                                                                , "eRByM"
                                                                , "eRByPT"
                                                                , "eRByTheta"
                                                                , "mBLTrue"
                                                                , "pTTrue"
                                                                , "cosTrue"
                                                                , "MET"
                                                                ]

      parseCalcSave' :: C.ByteString -> Handle -> StateT Integer IO ()
      parseCalcSave' s h = do
            modify (+1)

            case parse parseEvent s of
              Fail r _ _               -> liftIO $ C.putStr r
              Done evRemained evParsed ->
                  do let parmap = snd evParsed
                     printResult parmap h
                     parseCalcSave' evRemained h

printResult :: ParticleMap -> Handle -> StateT Integer IO ()
printResult pm hdl = do
  neve <- get
  let result = sequence [ eRatioTrue
                        , eRatioByM
                        , eRatioByPT
                        , eRatioByTheta
                        , mBLTrue
                        , pTTrue
                        , cosThetaTrue
                        , missingET
                        ] pm
  liftIO $ B.hPutStrLn hdl $
         B.pack (show neve ++ ", ") `B.append` B.intercalate ", " result

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
