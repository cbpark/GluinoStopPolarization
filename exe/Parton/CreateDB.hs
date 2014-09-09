{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Exception          (IOException, catch, finally)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map                   as M
import           Options.Applicative
import           System.Directory           (getTemporaryDirectory, removeFile)
import           System.Exit                (ExitCode (..))
import           System.IO
import           System.Process

import           Interface.IOHelper         (removeIfExists)
import           Parton.Variables           (var)

data Args = Args { input :: String, output :: String }

cmdoptions :: Parser Args
cmdoptions = Args <$> argument str ( metavar "DATA"
                                  <> help "Input DATA file in csv format") <*>
                      strOption ( long "output"
                               <> short 'o'
                               <> metavar "DB"
                               <> help "Output DB file to save the result" )

sqlCommand :: FilePath -> String
sqlCommand filename = unlines
    [ "CREATE TABLE var (neve INTEGER PRIMARY KEY" ++
      concatMap (\v -> ", " ++ C.unpack v ++ " REAL") (M.keys var)
      ++ ");"
    , ".separator \',\'"
    , ".import " ++ filename ++ " var"
    ]

importData :: Args -> IO ()
importData (Args infile outfile) = do
  removeIfExists outfile
  (Just hin, _, _, ph) <- createProcess (proc "sqlite3" [outfile])
                          { std_in = CreatePipe }
  (tempfile, temphandle) <- tempData infile
  finally ( do hPutStr hin $ sqlCommand tempfile
               hClose temphandle )
          ( do exitcode <- waitForProcess ph
               putStrLn $ case exitcode of
                            ExitFailure r -> "Error occurred while importing (" ++
                                             show r ++ ")."
                            ExitSuccess   -> outfile ++ " has been created."
               removeFile tempfile )
      where tempData :: FilePath -> IO (FilePath, Handle)
            tempData filename = do
              inputh <- openFile filename ReadMode
              contents <- C.hGetContents inputh
              tmpdir <- getTemporaryDirectory `catch`
                        (\(_ :: IOException) -> return ".")
              (tempf, temph) <- openTempFile tmpdir filename
              C.hPutStr temph $ (C.unlines . tail . C.lines) contents
              hClose inputh
              return (tempf, temph)

main :: IO ()
main =
  execParser opts >>= importData
      where opts = info (helper <*> cmdoptions)
                   ( fullDesc
                  <> progDesc "Import the data file into the SQLite DB"
                  <> header "GluinoStop_createdb - create db and import data" )
