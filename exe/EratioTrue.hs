{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Options.Applicative
import           Database.HDBC
import           Database.HDBC.Sqlite3           (connectSqlite3)
import Data.Maybe

data Args = Args { input :: [String], output :: String }

cmdoptions :: Parser Args
cmdoptions = Args <$> some (argument str ( metavar "INPUTS"
                                        <> help "Input DB files")) <*>
                      strOption ( long "output"
                               <> metavar "DATA"
                               <> help "Output data file to save the result" )

mkHist :: Args -> IO ()
mkHist (Args infiles outfile) = do
  vss <- mapM (query "er_true" "mbl_true > 0") infiles
  mapM_ print vss
  putStrLn $ "Output file: " ++ outfile

query :: String -> String -> FilePath -> IO [Double]
query var cut infile = do
  conn <- connectSqlite3 infile
  v <- quickQuery' conn ("SELECT " ++ var ++ " from var where " ++ cut) []
  let rows = map convRow v
  disconnect conn
  return $ map (fromMaybe (-10)) rows

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
