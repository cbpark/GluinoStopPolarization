{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Interface.Histogram1D (mkHist) where

import           Interface.Database                (queryVar)
import           Interface.IOHelper                (removeIfExists)

import qualified Data.ByteString.Char8             as B
import           Data.Double.Conversion.ByteString (toShortest)
import           Data.List                         (intercalate, transpose)
import qualified Data.Map                          as Map
import qualified Data.Vector                       as V
import qualified Data.Vector.Generic               as G
import           Statistics.Sample.Histogram       (histogram_)
import           System.IO                         (IOMode (..), withFile)

data HistFill = HistFill { cutStr     :: String
                         , nBin       :: Int
                         , lowerBound :: Double
                         , upperBound :: Double
                         }

histData :: Map.Map String HistFill
histData = Map.fromList [ ("er_true",       HistFill "er_true > 0 AND dr_true > 0.4" 50 0 1)
                        , ("er_by_m",       HistFill "er_by_m > 0" 50 0 1)
                        , ("er_by_pt",      HistFill "er_by_pt > 0" 50 0 1)
                        , ("er_by_theta",   HistFill "er_by_theta > 0" 50 0 1)
                        , ("er_by_r",       HistFill "er_by_r > 0" 50 0 1)
                        , ("mbl_true",      HistFill "mbl_true > 0" 100 0 200)
                        , ("pt_true",       HistFill "pt_true > 0" 200 0 2000)
                        , ("cos_theta_true",HistFill "mbl_true > 0" 200 (-10) 10)
                        , ("dr_true",       HistFill "dr_true > 0" 100 0 10)
                        ]

mkHist :: [FilePath] -> FilePath -> String -> IO ()
mkHist infiles outfile var = do
  removeIfExists outfile

  case Map.lookup var histData of
    Nothing              -> do putStrLn $ var ++ " is not known."
                               putStrLn $ "Possible variables: " ++
                                        intercalate ", " (Map.keys histData)
    Just (HistFill {..}) -> do
      result <- mkHist' infiles var cutStr nBin lowerBound upperBound
      withFile outfile WriteMode $ \hdl -> do
                                 writeHeader hdl
                                 mapM_ (B.hPutStrLn hdl) result
      putStrLn $ outfile ++ " has been created."
          where writeHeader h = B.hPutStrLn h $
                                "# " `B.append`
                                B.intercalate ", " (map B.pack infiles)

mkHist' :: [FilePath]        -- ^Input files
        -> String            -- ^Variable
        -> String            -- ^Cut string
        -> Int               -- ^Number of bins
        -> Double            -- ^Lower bound
        -> Double            -- ^Upper bound
        -> IO [B.ByteString]
mkHist' infiles var cut nbin lower upper = do
  vss <- mapM (queryVar var cut) infiles
  let hists = map (map toShortest . V.toList . histogram_ nbin lower upper) vss
      bins = map toShortest $ lowerBounds nbin lower upper
      result = transpose (bins : hists)
  return $ map (B.intercalate "  ") result

lowerBounds :: Int -> Double -> Double -> [Double]
lowerBounds nbin lower upper = V.toList $ G.generate nbin step
    where step i = lower + d * (fromIntegral i + 0.5)
          d = (upper - lower) / fromIntegral nbin
