{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class          (liftIO)
import           Control.Monad.Trans.State
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import qualified Data.ByteString.Char8           as B
import qualified Data.ByteString.Lazy.Char8      as C
import qualified Data.Map                        as M
import           Options.Applicative
import           System.IO                       (Handle, IOMode (..), withFile)

import           HEP.Data.LHEF
import           HEP.Data.LHEF.Parser

import           Interface.IOHelper              (removeIfExists)
import           Parton.Variables                (var)

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
  ntot <- withFile outfile WriteMode $ \hdl -> do
                                 writeHeader hdl
                                 execStateT ((parseCalcSave' . stripLHEF)
                                             evstr hdl) 0
  putStrLn $ "Total number of events parsed = " ++ show (ntot - 1)
    where
      writeHeader :: Handle -> IO ()
      writeHeader h = C.hPutStrLn h $
                      "# " `C.append` C.intercalate ", "
                      (M.keys var)

      parseCalcSave' :: C.ByteString -> Handle -> StateT Integer IO ()
      parseCalcSave' s h = do
        modify (+1)
        case parse lhefEvent s of
          Fail r _ _               -> liftIO $ C.putStr r
          Done evRemained evParsed -> do printResult (snd evParsed) h
                                         parseCalcSave' evRemained h

printResult :: ParticleMap -> Handle -> StateT Integer IO ()
printResult pm hdl = do
  neve <- get
  let result = sequence (M.elems var) pm
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
