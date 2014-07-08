{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Interface.IOHelper              (removeIfExists)
import           Parton.Variables                (varParton)
import           Jet.Variables (varJet)

import           HEP.Data.LHEF
import           HEP.Data.LHEF.Parser

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.State
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as C
import           Data.Char                       (toLower)
import qualified Data.Map                        as Map
import           Options.Applicative
import           System.IO                       (Handle, IOMode (..), withFile)

data Args = Args { input :: String, datatype :: String, output :: String }

data EventType = Parton | Jet deriving Eq

cmdoptions :: Parser Args
cmdoptions = Args <$> argument str ( metavar "LHEF"
                                  <> help "Input LHEF file") <*>
                      strOption ( long "event"
                               <> short 'e'
                               <> metavar "EVENT"
                               <> help "Event type (either parton or jet)" ) <*>
                      strOption ( long "output"
                               <> short 'o'
                               <> metavar "OUTPUT"
                               <> help "Output file to save the result" )

calcAndSave :: Args -> IO ()
calcAndSave (Args infile etype outfile) =
    case map toLower etype of
      "parton" -> parseCalcSave infile outfile Parton
      "jet"    -> parseCalcSave infile outfile Jet
      _        -> putStrLn $ "Event type " ++ etype ++ " is not available."

parseCalcSave :: FilePath -> FilePath -> EventType -> IO ()
parseCalcSave infile outfile etype = do
  removeIfExists outfile
  evstr <- C.readFile infile
  ntot <- withFile outfile WriteMode $ \hdl -> do
                                 writeHeader hdl etype
                                 execStateT ((parseCalcSave' . stripLHEF)
                                             evstr hdl etype) 0
  C.putStrLn . C.pack $ "Total number of events parsed = " ++ show (ntot - 1)
    where
      writeHeader :: Handle -> EventType -> IO ()
      writeHeader h et = C.hPutStrLn h $
                         "# " `C.append` C.intercalate ", "
                         (Map.keys $ case et of Parton -> varParton
                                                Jet    -> varJet)

      parseCalcSave' :: C.ByteString -> Handle -> EventType -> StateT Integer IO ()
      parseCalcSave' s h et = do
            modify (+1)
            case parse parseEvent s of
              Fail r _ _               -> liftIO $ C.putStr r
              Done evRemained evParsed -> do
                                      printResult (snd evParsed) h et
                                      parseCalcSave' evRemained h et

printResult :: ParticleMap -> Handle -> EventType -> StateT Integer IO ()
printResult pm hdl et = do
  neve <- get
  let result = sequence (Map.elems $ case et of Parton -> varParton
                                                Jet    -> varJet) pm
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
