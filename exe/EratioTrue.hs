{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import           Database.HDBC
import           Database.HDBC.Sqlite3       (connectSqlite3)
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
  vss <- mapM (query "er_true" "mbl_true > 0") infiles
  let hists = map (mkHist' 100 0 1) vss
  mapM_ print hists
  putStrLn $ "Output file: " ++ outfile

mkHist' :: Int -> Double -> Double -> V.Vector Double -> V.Vector (Double, Double)
mkHist' nbin lower upper xs = V.zip bins hist
    where bins = G.generate nbin step
          step i = lower + d * fromIntegral i
          d = (upper - lower) / fromIntegral nbin
          hist = histogram_ nbin lower upper xs

query :: String -> String -> FilePath -> IO (V.Vector Double)
query var cut infile = do
  conn <- connectSqlite3 infile
  v <- quickQuery' conn ("SELECT " ++ var ++ " from var where " ++ cut) []
  let rows = map convRow v
  disconnect conn
  return . V.fromList $ map (fromMaybe (-10)) rows
      where convRow :: [SqlValue] -> Maybe Double
            convRow [sqlVal] = (return . fromSql) sqlVal
            convRow _        = Nothing

main :: IO ()
main =
  execParser opts >>= mkHist
      where opts = info (helper <*> cmdoptions)
                   ( fullDesc
                  <> progDesc "Create the histogram"
                  <> header "GluinoStop_er_true - create a histogram for er_true" )
