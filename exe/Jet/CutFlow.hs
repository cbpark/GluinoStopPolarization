module Main where

import           Interface.Database  (queryCount)

import           Control.Monad
import           Data.List           (intercalate)
import qualified Data.Map            as Map
import           Options.Applicative
import           System.Directory    (doesFileExist)

data Args = Args { cut :: [String] }

cmdoptions :: Parser Args
cmdoptions = Args <$> some (argument str ( metavar "CUTS"
                                        <> help "selection cuts"))

scale :: Map.Map String Double
scale = Map.fromList [ ("Gluino_LHStop_semileptonic", 3.113951639514e-2)
                     , ("Gluino_RHStop_semileptonic", 3.088337466216e-2)
                     , ("ttbb", 0.88208)
                     , ("ttz", 0.5519556)
                     , ("tth", 0.8639613333333334)
                     , ("tzj", 1.21201912)
                     , ("tbzj", 1.0080504)
                     ]

cutMap :: Map.Map String String
cutMap = Map.fromList [ ("basic", "nl == 1 AND nb >= 3")
                      , ("met", "met > 150")
                      ]

countEvents :: Args -> IO ()
countEvents (Args cuts) = do
  let unknown = filter (`Map.notMember` cutMap) cuts
  if (not . null) unknown
  then do mapM_ (putStrLn . (++" is not valid.")) unknown
          putStrLn $ "Cut variables: " ++ intercalate ", " (Map.keys cutMap)
  else countEvents' cuts
    where countEvents' cs = do let cs' = map (\c -> cutMap Map.! c) cs
                                   cutstr = intercalate " AND " cs'
                               countMap <- getCount cutstr
                               mapM_ (\(k, v) -> putStrLn $ k ++ ": " ++ show v)
                                     (Map.toList countMap)

getCount :: String -> IO (Map.Map String Double)
getCount cuts = liftM Map.fromList $ mapM getCount' (Map.keys scale)
    where getCount' :: String -> IO (String, Double)
          getCount' dname = do
            let datafile = dname ++ "_jet.db"
            fileExists <- doesFileExist datafile
            if fileExists
            then do n <- queryCount cuts datafile
                    return (dname, fromIntegral n * (scale Map.! dname))
            else do putStrLn $ "-- " ++ datafile ++ " does not exist."
                    return (dname, 0)

main :: IO ()
main = execParser opts >>= countEvents
    where opts = info (helper <*> cmdoptions)
                 ( fullDesc
                <> progDesc "Count number of events for given selection cuts"
                <> header "GluinoStop_cut" )
