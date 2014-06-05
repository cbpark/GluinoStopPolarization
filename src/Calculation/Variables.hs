module Calculation.Variables where

import           HEP.Data.LHEF

import           Calculation.ParSelector

import           Control.Monad              (filterM)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT (..), ask)

eRatioBLTrue :: ParticleMap -> Double
eRatioBLTrue pm = case eRatioBL (particlesFromTop pm) of
                    Nothing    -> 0
                    Just (r:_) -> r

eRatioBL :: [[Particle]] -> Maybe [Double]
eRatioBL pss = do
  -- The elements of blpairs are guaranteed to have exactly one pair of
  -- b-quark and lepton or nothing by containsBL.
  blpairs <- filterM containsBL pss
  mapM (runReaderT eRatioBL') blpairs

eRatioBL' :: ReaderT [Particle] Maybe Double
eRatioBL' = do eLepton <- theEnergy lepton
               eBquark <- theEnergy bQuark
               return $ eLepton / (eBquark + eLepton)
    where theEnergy :: [Int] -> ReaderT [Particle] Maybe Double
          theEnergy ns = do ps <- ask
                            case filter (inParticles ns) ps of
                              []    -> lift Nothing
                              (p:_) -> return (energyOf p)
