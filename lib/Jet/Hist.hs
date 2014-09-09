module Jet.Hist
    (
      histData
    , mkHist
    ) where

import           Data.Map              (Map, fromList)

import           Interface.Histogram1D

basicCut :: String
basicCut = " AND nl >= 1 AND nb >= 3 AND nj >= 4 AND pTj1 > 90 AND met > 150"

histData :: Map String HistFill
histData = fromList [ ("m_bl_theta",
                       HistFill ("m_bl_theta > 0 AND m_bl_theta < 1000" ++ basicCut)
                       100 0 1000)
                    , ("mT",
                       HistFill ("mT > 0 AND mT < 1000" ++ basicCut)
                       50 0 1000)
                    , ("meff",
                       HistFill ("meff > 0 AND meff < 5000" ++ basicCut)
                       50 0 5000)
                    , ("met",
                       HistFill ("met > 0 AND met < 1000" ++ basicCut)
                       50 0 1000)
                    , ("nj",
                       HistFill ("nj > 0 AND nj < 20" ++ basicCut)
                       20 0 20)
                    ]
