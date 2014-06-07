module Calculation.Variables
    (
    -- * Energy ratio of b quark and lepton
      eRatioBLTruePair
    ) where

import           HEP.Data.LHEF

import           Calculation.ParSelector

import           Control.Monad                     (filterM)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Reader        (Reader, ReaderT (..), ask,
                                                    runReader)
import           Data.ByteString.Char8             (ByteString)
import           Data.Double.Conversion.ByteString (toFixed)

eRatioBLTruePair :: ParticleMap -> ByteString
eRatioBLTruePair = runReader $ eRatioBLpair particlesFromTop

eRatioBLpair :: (ParticleMap -> ParticlePairs)
             -> Reader ParticleMap ByteString
eRatioBLpair getPair = do
  pm <- ask
  let e = case (eRatioBLs . getPair) pm of Nothing    -> 0
                                           Just []    -> 0
                                           Just (r:_) -> r
  return $ toFixed 3 e

eRatioBLs :: ParticlePairs -> Maybe [Double]
eRatioBLs pss = mapM (runReaderT eRatioBLs') =<< filterM containsBL pss

eRatioBLs' :: ReaderT [Particle] Maybe Double
eRatioBLs' = do eLepton <- theEnergyOf lepton
                eBquark <- theEnergyOf bQuark
                return $ eLepton / (eBquark + eLepton)
    where theEnergyOf :: ParType -> ReaderT [Particle] Maybe Double
          theEnergyOf pt = do ps <- ask
                              case filter (inParticles pt) ps of
                                []    -> lift Nothing
                                (p:_) -> return (energyOf p)
