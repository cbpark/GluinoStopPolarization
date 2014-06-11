module Calculation.Variables
    (
    -- * Energy ratio of b quark and lepton
      eRatioBLTrue
    , eRatioBLbyM
    , eRatioBLbyPT
    , eRatioBLbyTheta
    )
    where

import           Calculation.ParSelector

import           HEP.Data.LHEF
import           HEP.Vector
import           HEP.Vector.LorentzVector

import           Control.Monad                     (filterM)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Reader        (Reader, ReaderT (..), ask,
                                                    runReader)
import           Data.ByteString.Char8             (ByteString)
import           Data.Double.Conversion.ByteString (toFixed)
import           Data.Function                     (on)
import qualified Data.Map                          as Map

eRatioBLTrue :: ParticleMap -> ByteString
eRatioBLTrue = runReader $ eRatioBLpair particlesFromTop

eRatioBLbyM :: ParticleMap -> ByteString
eRatioBLbyM = runReader $ eRatioBLpair pairByM

eRatioBLbyPT :: ParticleMap -> ByteString
eRatioBLbyPT = runReader $ eRatioBLpair pairByPT

eRatioBLbyTheta :: ParticleMap -> ByteString
eRatioBLbyTheta = runReader $ eRatioBLpair pairByTheta

eRatioBLpair :: (ParticleMap -> ParticlePairs)
             -> Reader ParticleMap ByteString
eRatioBLpair getPair = do
  pm <- ask
  let e = case (eRatioBLs . getPair) pm of Just (r:_) -> r
                                           Just []    -> -1
                                           Nothing    -> -1
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
                                (p:_) -> return (energyOf p)
                                []    -> lift Nothing

data Choice = ByMin | ByMax
data HowPair = HowPair ([Particle] -> Double) Choice

pairBy :: HowPair -> ParticleMap -> ParticlePairs
pairBy (HowPair func choice) pm =
    let allpairs = particlesOfAllBL pm
        pairMap = foldr (\p m -> Map.insert (func p) p m) Map.empty allpairs
        chosenPair = case choice of ByMin -> Map.minView pairMap
                                    _     -> Map.maxView pairMap
    in case chosenPair of Just (pair, _) -> [pair]
                          Nothing        -> []

pairByM :: ParticleMap -> ParticlePairs
pairByM = pairBy (HowPair invMass ByMin)
    where invMass = invariantMass . foldr ((.+.) . fourMomentum) zero

pairByPT :: ParticleMap -> ParticlePairs
pairByPT = pairBy (HowPair transMomentum ByMax)
    where transMomentum = pT . foldr ((.+.) . fourMomentum) zero

pairByTheta :: ParticleMap -> ParticlePairs
pairByTheta = pairBy (HowPair cosTheta ByMax)
    where cosTheta [p, p'] = cos $ (deltaTheta `on` fourMomentum) p p'
          cosTheta _       = -1
