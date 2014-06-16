module Main where

import           Interface.Database          (queryVar)

import           Data.List                   (transpose)
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import           Options.Applicative
import           Statistics.Sample.Histogram (histogram_)

data Args = Args { input :: [String], output :: String }

cmdoptions :: Parser Args
cmdoptions = Args <$> some (argument str ( metavar "INPUTS"
                                        <> help "Input DB files")) <*>
                      strOption ( long "output"
                               <> short 'o'
                               <> metavar "OUTPUT"
                               <> help "Output data file to save the result" )

mkHist :: Args -> IO ()
mkHist (Args infiles outfile) = do
  vss <- mapM (queryVar "er_true" "mbl_true > 0") infiles
  let nbin = 100
      lower = 0
      upper = 1
      hists = map (V.toList . histogram_ nbin lower upper) vss
      bins = lowerBounds nbin lower upper
      result = bins : hists
  mapM_ print (transpose result)
  putStrLn $ "Output file: " ++ outfile

lowerBounds :: Int -> Double -> Double -> [Double]
lowerBounds nbin lower upper = V.toList $ G.generate nbin step
    where step i = lower + d * fromIntegral i
          d = (upper - lower) / fromIntegral nbin

main :: IO ()
main =
  execParser opts >>= mkHist
      where opts = info (helper <*> cmdoptions)
                   ( fullDesc
                  <> progDesc "Create the histogram"
                  <> header "GluinoStop_er_true - create a histogram for er_true" )
