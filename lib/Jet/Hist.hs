module Jet.Hist
    (
      histData
    , mkHist
    ) where

import           Interface.Histogram1D

import qualified Data.Map              as Map

basicCut :: String
basicCut = " AND nl == 1 AND nb >= 3"

histData :: Map.Map String HistFill
histData = Map.fromList [ ("m_bl_theta",
                           HistFill ("m_bl_theta > 0 AND m_bl_theta < 1000" ++
                                     basicCut)
                           100 0 1000)
                        , ("mT",
                           HistFill ("mT > 0 AND mT < 1000" ++ basicCut)
                           100 0 1000)
                        , ("meff",
                           HistFill ("meff > 0 AND meff < 5000" ++ basicCut)
                           100 0 5000)
                        , ("met",
                           HistFill ("met > 0 AND met < 1000" ++ basicCut)
                           50 0 1000)
                        , ("nj",
                           HistFill ("nj > 0 AND nj < 20" ++ basicCut)
                           20 0 20)
                        ]
