{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Options.Applicative

data Args = Args { input :: [String], output :: String }

cmdoptions :: Parser Args
cmdoptions = Args <$> some (argument str ( metavar "INPUTS"
                                        <> help "Input DB files")) <*>
                      strOption ( long "output"
                               <> metavar "DATA"
                               <> help "Output data file to save the result" )

mkHist :: Args -> IO ()
mkHist (Args infiles outfile) = do
  mapM_ putStrLn infiles
  putStrLn $ "Output file: " ++ outfile

main :: IO ()
main =
  execParser opts >>= mkHist
      where opts = info (helper <*> cmdoptions)
                   ( fullDesc
                  <> progDesc "Create the histogram"
                  <> header "GluinoStop_er_true - create a histogram for er_true" )
