module Main where

import           Jet.Hist            (histData, mkHist)

import           Options.Applicative

data Args = Args { input :: [String], var :: String, output :: String }

cmdoptions :: Parser Args
cmdoptions = Args <$> some (argument str ( metavar "INPUTS"
                                        <> help "Input DB files")) <*>
                      strOption ( long "variable"
                               <> short 'v'
                               <> metavar "VARIABLE"
                               <> help "Variable name" ) <*>
                      strOption ( long "output"
                               <> short 'o'
                               <> metavar "OUTPUT"
                               <> help "Output data file to save the result" )

saveHist :: Args -> IO ()
saveHist (Args infiles variable outfile) = mkHist infiles outfile variable histData

main :: IO ()
main =
  execParser opts >>= saveHist
      where opts = info (helper <*> cmdoptions)
                   ( fullDesc
                  <> progDesc "Make the histogram data"
                  <> header "GluinoStop_hist1d - make a 1D histogram data" )
