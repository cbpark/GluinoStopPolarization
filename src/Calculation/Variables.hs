module Calculation.Variables where

import           HEP.Data.LHEF

import           Calculation.ParSelector

import           Control.Monad              (join)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT (..), ask)
import           Data.Maybe                 (fromMaybe)

eRatioBL :: [[Particle]] -> Double
eRatioBL pss =
    let blpairs = filter containsBL pss
        result = (join . safeHead . map (runReaderT eRatioBL')) blpairs
    in fromMaybe 0 result

eRatioBL' :: ReaderT [Particle] Maybe Double
eRatioBL' = do eLepton <- theEnergy lepton
               eBquark <- theEnergy bQuark
               return $ eLepton / (eBquark + eLepton)

theEnergy :: [Int] -> ReaderT [Particle] Maybe Double
theEnergy ns = do ps <- ask
                  case (safeHead . filter (inParticles ns)) ps of
                    Just p  -> return (energyOf p)
                    Nothing -> lift Nothing

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
