module Main where

import           Interface.Database        (queryCount)

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.List                 (intercalate)
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Options.Applicative
import           System.Directory          (doesFileExist)

data Args = Args { cut :: [String] }

cmdoptions :: Parser Args
cmdoptions = Args <$> some (argument str ( metavar "CUTS"
                                        <> help "selection cuts"))

signalLH :: Map String Double
signalLH = M.fromList [("Gluino_LHStop_semileptonic", 1.5380866629110083e-3)]

signalRH :: Map String Double
signalRH = M.fromList [("Gluino_RHStop_semileptonic", 1.5085951819250394e-3)]

bkgs :: Map String Double
bkgs = M.fromList [ ("ttbb", 0.88208)
                  , ("ttz", 0.5519556)
                  , ("tth", 0.8639613333333334)
                  , ("tzj", 0.8080128)
                  , ("tbzj", 0.6720336)
                  ]

cutMap :: Map String String
cutMap = M.fromList [ ("basic", "nl >= 1 AND nb >= 3 AND nj >= 4 AND pTj1 > 90 AND met > 150")
                    , ("met", "met > 175")
                    , ("mT", "mT > 140")
                    , ("meff", "meff > 700")
                    ]

countEvents :: Args -> IO ()
countEvents (Args cuts) = do
  let unknown = filter (`M.notMember` cutMap) cuts
  if (not . null) unknown
  then do mapM_ (putStrLn . (++" is not valid.")) unknown
          putStrLn $ "Cut variables: " ++ intercalate ", " (M.keys cutMap)
  else do nbkg <- printResult bkgs
          nLH <- printResult signalLH
          putStrLn $ "Signal significance: " ++ show (sigEstimator nLH nbkg)
          nRH <- printResult signalRH
          putStrLn $ "Signal significance: " ++ show (sigEstimator nRH nbkg)
    where
      printResult :: Map String Double -> IO Double
      printResult evs = do
        putStrLn "-------------------------------------------------------------"
        n <- execStateT (countEvents' cuts evs) 0
        putStrLn $ "# of events passed = " ++ show n
        return n

      countEvents' :: [String] -> Map String Double -> StateT Double IO ()
      countEvents' cs es = do let cs' = map (\c -> cutMap M.! c) cs
                                  cutstr = intercalate " AND " cs'
                              countMap <- liftIO $ getCount cutstr es
                              modify (+ (sum $ M.elems countMap))
                              liftIO $ mapM_ (\(k, v) ->
                                              putStrLn $ k ++ ": " ++ show v)
                                             (M.toList countMap)

sigEstimator :: Double -> Double -> Double
sigEstimator nsig nbkg =
    sqrt $ 2 * (nsig + nbkg) * log (1 + nsig / nbkg) - 2 * nsig

getCount :: String -> Map String Double -> IO (Map String Double)
getCount cuts evs = liftM M.fromList $ mapM getCount' (M.keys evs)
    where getCount' :: String -> IO (String, Double)
          getCount' dataname = do
            let datafile = dataname ++ "_jet.db"
            fileExists <- doesFileExist datafile
            if fileExists
            then do n <- queryCount cuts datafile
                    return (dataname, fromIntegral n * (evs M.! dataname))
            else do putStrLn $ "-- " ++ datafile ++ " does not exist."
                    return (dataname, 0)

main :: IO ()
main = execParser opts >>= countEvents
    where opts = info (helper <*> cmdoptions)
                 ( fullDesc
                <> progDesc "Count number of events for given selection cuts"
                <> header "GluinoStop_cut" )
