{-# LANGUAGE OverloadedStrings #-}

module Jet.Hist
    (
      histData
    , mkHist
    ) where

import           Interface.Histogram1D

import qualified Data.Map              as Map

histData :: Map.Map String HistFill
histData = Map.fromList [ ("m_bl_theta",
                           HistFill ("m_bl_theta > 0 AND m_bl_theta < 1000 " ++
                                     "AND nl == 1 AND nb >= 3")
                           200 0 1000) ]
