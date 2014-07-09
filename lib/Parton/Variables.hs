{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Parton.Variables
    (
      var
    , varallcomb

    -- * Energy ratio of b quark and lepton
    , eRatioTrue
    , eRatioByM
    , eRatioByPT
    , eRatioByTheta

    -- * Invariant mass of b quark and lepton
    , mBLTrue
    , mBLAll

    -- * Transverse momentum of b quark and lepton
    , pTTrue
    , pTAll

    -- * cos(theta) of b quark and lepton
    , cosThetaTrue
    , cosThetaAll

    -- * Delta R of b quark and lepton
    , deltaRTrue

    -- * Missing transverse momentum
    , missingET
    ) where

import           Object.Particles
import           Parton.Selection

import           HEP.Data.LHEF

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.ByteString.Char8             (ByteString)
import qualified Data.ByteString.Lazy.Char8        as C
import           Data.Double.Conversion.ByteString (toFixed)
import           Data.List                         (find)
import qualified Data.Map                          as Map
import           Data.Maybe                        (mapMaybe)

var :: Map.Map C.ByteString (ParticleMap -> ByteString)
var = Map.fromList $ zip varField varFuncs

varField :: [C.ByteString]
varField = [ "er_true"
           , "er_by_m"
           , "er_by_pt"
           , "er_by_theta"
           , "er_by_r"
           , "mbl_true"
           , "pt_true"
           , "cos_theta_true"
           , "dr_true"
           , "met"
           ]

varFuncs :: [ParticleMap -> ByteString]
varFuncs = [ eRatioTrue
           , eRatioByM
           , eRatioByPT
           , eRatioByTheta
           , eRatioByR
           , mBLTrue
           , pTTrue
           , cosThetaTrue
           , deltaRTrue
           , missingET
           ]

varallcomb :: Map.Map C.ByteString (ParticleMap -> [ByteString])
varallcomb = Map.fromList $ zip varallcombField varallcombFuncs

varallcombField :: [C.ByteString]
varallcombField = [ "mbl_all"
                  , "pt_all"
                  , "cos_theta_all"
                  , "dr_all"
                  ]

varallcombFuncs :: [ParticleMap -> [ByteString]]
varallcombFuncs = [ mBLAll
                  , pTAll
                  , cosThetaAll
                  , deltaRAll
                  ]

eRatioTrue :: ParticleMap -> ByteString
eRatioTrue = runReader $ eRatioBLpair particlesFromTop

eRatioByM :: ParticleMap -> ByteString
eRatioByM = runReader $ eRatioBLpair pairByM

eRatioByPT :: ParticleMap -> ByteString
eRatioByPT = runReader $ eRatioBLpair pairByPT

eRatioByTheta :: ParticleMap -> ByteString
eRatioByTheta = runReader $ eRatioBLpair pairByTheta

eRatioByR :: ParticleMap -> ByteString
eRatioByR = runReader $ eRatioBLpair pairByR

eRatioBLpair :: (ParticleMap -> ParticlePairs)
             -> Reader ParticleMap ByteString
eRatioBLpair getPair = do
  pm <- ask
  let e = case (eRatioBL . getPair) pm of (r:_) -> r
                                          _     -> -1
  return $ toFixed 3 e

eRatioBL :: ParticlePairs -> [Double]
eRatioBL = mapMaybe (runReaderT eRatioBL') . filter containsBL
    where
      eRatioBL' :: ReaderT [Particle] Maybe Double
      eRatioBL' = do eLepton <- theEnergyOf lepton
                     eBquark <- theEnergyOf bQuark
                     return $ eLepton / (eBquark + eLepton)

      theEnergyOf :: ParticleType -> ReaderT [Particle] Maybe Double
      theEnergyOf par = do ps <- ask
                           case find (`is` par) ps of
                             Just p  -> return (energyOf p)
                             _       -> lift Nothing

data Choice = ByMin | ByMax
data HowPair = HowPair ([Particle] -> Maybe Double) Choice

pairByM :: ParticleMap -> ParticlePairs
pairByM = pairBy (HowPair (Just . invMass) ByMin)

pairByPT :: ParticleMap -> ParticlePairs
pairByPT = pairBy (HowPair (Just . transMomentum) ByMax)

pairByTheta :: ParticleMap -> ParticlePairs
pairByTheta = pairBy (HowPair cosTheta ByMax)

pairByR :: ParticleMap -> ParticlePairs
pairByR = pairBy (HowPair dR ByMin)

pairBy :: HowPair -> ParticleMap -> ParticlePairs
pairBy (HowPair func choice) pm =
    let allpairs = particlesOfAllBL pm
        pairMap = foldr (\p m -> Map.insert (func p) p m) Map.empty allpairs
        chosenPair = (\case ByMax -> Map.maxView pairMap
                            _     -> Map.minView pairMap) choice
    in case chosenPair of Just (pair, _) -> [pair]
                          _              -> []

mBLTrue :: ParticleMap -> ByteString
mBLTrue = headOf . runReader (calcVar 2 particlesFromTop (Just . invMass))

mBLAll :: ParticleMap -> [ByteString]
mBLAll = runReader $ calcVar 2 particlesOfAllBL (Just . invMass)

pTTrue :: ParticleMap -> ByteString
pTTrue = headOf . runReader (calcVar 2 particlesFromTop (Just . transMomentum))

pTAll :: ParticleMap -> [ByteString]
pTAll = runReader $ calcVar 2 particlesOfAllBL (Just . transMomentum)

cosThetaTrue :: ParticleMap -> ByteString
cosThetaTrue = headOf . runReader (calcVar 3 particlesFromTop cosTheta)

cosThetaAll :: ParticleMap -> [ByteString]
cosThetaAll = runReader $ calcVar 3 particlesOfAllBL cosTheta

deltaRTrue :: ParticleMap -> ByteString
deltaRTrue = headOf . runReader (calcVar 3 particlesFromTop dR)

deltaRAll :: ParticleMap -> [ByteString]
deltaRAll = runReader $ calcVar 3 particlesOfAllBL dR

calcVar :: Int -> (ParticleMap -> ParticlePairs) -> ([Particle] -> Maybe Double)
        -> Reader ParticleMap [ByteString]
calcVar n mkpair func = do
  pm <- ask
  return $ map (toFixed n) $ runReader (calcVar' func) pm
    where
      calcVar' :: ([Particle] -> Maybe Double) -> Reader ParticleMap [Double]
      calcVar' f = do
        pm' <- ask
        let pss = filter containsBL (mkpair pm')
        return $ mapMaybe f pss

missingET :: ParticleMap -> ByteString
missingET = toFixed 2 .
            transMomentum . filter (`is` invisible) . runReader finalStates

headOf :: [ByteString] -> ByteString
headOf (x:_) = x
headOf _     = "-10"
