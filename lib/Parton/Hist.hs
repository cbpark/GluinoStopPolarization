{-# LANGUAGE OverloadedStrings #-}

module Parton.Hist
    (
      histData
    , mkHist
    ) where

import           Data.Map              (Map, fromList)

import           Interface.Histogram1D

histData :: Map String HistFill
histData = fromList [ ("er_true",       HistFill "er_true > 0 AND dr_true > 0.4" 50 0 1)
                    , ("er_by_m",       HistFill "er_by_m > 0" 50 0 1)
                    , ("er_by_pt",      HistFill "er_by_pt > 0" 50 0 1)
                    , ("er_by_theta",   HistFill "er_by_theta > 0" 50 0 1)
                    , ("er_by_r",       HistFill "er_by_r > 0" 50 0 1)
                    , ("mbl_true",      HistFill "mbl_true > 0" 100 0 200)
                    , ("pt_true",       HistFill "pt_true > 0" 200 0 2000)
                    , ("cos_theta_true",HistFill "mbl_true > 0" 200 (-10) 10)
                    , ("dr_true",       HistFill "dr_true > 0" 100 0 10)
                    ]
