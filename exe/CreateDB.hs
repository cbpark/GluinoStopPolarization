module Main where

import           Calculation.Variables      (variables)

import           Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as C

data Args = Args { input :: String, output :: String }

cmdoptions :: Parser Args
cmdoptions = Args <$> strOption ( long "input"
                               <> metavar "DATA"
                               <> help "Input DATA file in csv format" ) <*>
                      strOption ( long "output"
                               <> metavar "DB"
                               <> help "Output DB file to save the result" )

importData :: FilePath -> String
importData filename = unlines
    [ "Create TABLE var (neve INTEGER PRIMARY KEY" ++
      concatMap (\v -> ", " ++ C.unpack v ++ " REAL") (tail variables) ++ ");"
    , ".separator \',\'"
    , ".import " ++ filename ++ " var"
    ]

importAndSave :: Args -> IO ()
importAndSave (Args x y) = do
  putStrLn x
  putStrLn y
  putStrLn $ importData "test"

main :: IO ()
main =
  execParser opts >>= importAndSave
      where opts = info (helper <*> cmdoptions)
                   ( fullDesc
                  <> progDesc "Import the data file into the SQLite DB"
                  <> header "GluinoStop_createdb - create db and import data" )
