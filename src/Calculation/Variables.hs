module Calculation.Variables
    (
    -- * Energy ratio of b quark and lepton
      eRatioTrue
    , eRatioByM
    , eRatioByPT
    , eRatioByTheta

    -- * Invariant mass of b quark and lepton
    , mBLTrue

    -- * Missing transverse momentum
    , missingET
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
import           Data.List                         (find)
import qualified Data.Map                          as Map

eRatioTrue :: ParticleMap -> ByteString
eRatioTrue = runReader $ eRatioBLpair particlesFromTop

eRatioByM :: ParticleMap -> ByteString
eRatioByM = runReader $ eRatioBLpair pairByM

eRatioByPT :: ParticleMap -> ByteString
eRatioByPT = runReader $ eRatioBLpair pairByPT

eRatioByTheta :: ParticleMap -> ByteString
eRatioByTheta = runReader $ eRatioBLpair pairByTheta

eRatioBLpair :: (ParticleMap -> ParticlePairs)
             -> Reader ParticleMap ByteString
eRatioBLpair getPair = do
  pm <- ask
  let e = case (eRatioBL . getPair) pm of Just (r:_) -> r
                                          _          -> -1
  return $ toFixed 3 e

eRatioBL :: ParticlePairs -> Maybe [Double]
eRatioBL pss = filterM containsBL pss >>= mapM (runReaderT eRatioBL')
    where
      eRatioBL' :: ReaderT [Particle] Maybe Double
      eRatioBL' = do eLepton <- theEnergyOf lepton
                     eBquark <- theEnergyOf bQuark
                     return $ eLepton / (eBquark + eLepton)

      theEnergyOf :: ParType -> ReaderT [Particle] Maybe Double
      theEnergyOf par = do ps <- ask
                           case find (`is` par) ps of
                             Just p  -> return (energyOf p)
                             Nothing -> lift Nothing

data Choice = ByMin | ByMax
data HowPair = HowPair ([Particle] -> Double) Choice

pairBy :: HowPair -> ParticleMap -> ParticlePairs
pairBy (HowPair func choice) pm =
    let allpairs = particlesOfAllBL pm
        pairMap = foldr (\p m -> Map.insert (func p) p m) Map.empty allpairs
        chosenPair = case choice of ByMax -> Map.maxView pairMap
                                    ByMin -> Map.minView pairMap
    in case chosenPair of Just (pair, _) -> [pair]
                          _              -> []

pairByM :: ParticleMap -> ParticlePairs
pairByM = pairBy (HowPair invMass ByMin)

invMass :: [Particle] -> Double
invMass = invariantMass . foldr ((.+.) . fourMomentum) zero

pairByPT :: ParticleMap -> ParticlePairs
pairByPT = pairBy (HowPair transMomentum ByMax)

transMomentum :: [Particle] -> Double
transMomentum = pT . foldr ((.+.) . fourMomentum) zero

pairByTheta :: ParticleMap -> ParticlePairs
pairByTheta = pairBy (HowPair cosTheta ByMax)
    where cosTheta [p, p'] = cos $ (deltaTheta `on` fourMomentum) p p'
          cosTheta _       = -1

mBLTrue :: ParticleMap -> ByteString
mBLTrue pm = toFixed 2 $ case mBLTrue' pm of Just (m:_) -> m
                                             _          -> -1
    where mBLTrue' :: ParticleMap -> Maybe [Double]
          mBLTrue' pm' = do
            let pss = particlesFromTop pm'
            pss' <- filterM containsBL pss
            return $ map invMass pss'

missingET :: ParticleMap -> ByteString
missingET = toFixed 2 . transMomentum . filter (`is` invisible) . finalStates
