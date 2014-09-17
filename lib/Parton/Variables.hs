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

import           Control.Monad                     (liftM)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.ByteString.Char8             (ByteString)
import qualified Data.ByteString.Lazy.Char8        as C
import           Data.Double.Conversion.ByteString (toFixed)
import qualified Data.Foldable                     as F
import           Data.Map                          (Map)
import qualified Data.Map                          as M
import           Data.Maybe                        (mapMaybe)
import           Data.Sequence                     (Seq)
import qualified Data.Sequence                     as S

import           HEP.Data.LHEF

import           Object.Particles
import           Parton.Selection

var :: Map C.ByteString (EventEntry -> ByteString)
var = M.fromList $ zip varField varFuncs

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

varFuncs :: [EventEntry -> ByteString]
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

varallcomb :: Map C.ByteString (EventEntry -> [ByteString])
varallcomb = M.fromList $ zip varallcombField varallcombFuncs

varallcombField :: [C.ByteString]
varallcombField = [ "mbl_all"
                  , "pt_all"
                  , "cos_theta_all"
                  , "dr_all"
                  ]

varallcombFuncs :: [EventEntry -> [ByteString]]
varallcombFuncs = [ mBLAll
                  , pTAll
                  , cosThetaAll
                  , deltaRAll
                  ]

eRatioTrue :: EventEntry -> ByteString
eRatioTrue = runReader $ eRatioBLpair particlesFromTop

eRatioByM :: EventEntry -> ByteString
eRatioByM = runReader $ eRatioBLpair pairByM

eRatioByPT :: EventEntry -> ByteString
eRatioByPT = runReader $ eRatioBLpair pairByPT

eRatioByTheta :: EventEntry -> ByteString
eRatioByTheta = runReader $ eRatioBLpair pairByTheta

eRatioByR :: EventEntry -> ByteString
eRatioByR = runReader $ eRatioBLpair pairByR

eRatioBLpair :: (EventEntry -> ParticlePairs) -> Reader EventEntry ByteString
eRatioBLpair getPair = do
  pm <- ask
  let e = case (eRatioBL . getPair) pm of (r:_) -> r
                                          _     -> -1
  return $ toFixed 3 e

eRatioBL :: ParticlePairs -> [Double]
eRatioBL = mapMaybe (runReaderT eRatioBL' . F.toList) . filter containsBL
    where
      eRatioBL' :: ReaderT [Particle] Maybe Double
      eRatioBL' = do eLepton <- theEnergyOf lepton
                     eBquark <- theEnergyOf bQuark
                     return $ eLepton / (eBquark + eLepton)

      theEnergyOf :: ParticleType -> ReaderT [Particle] Maybe Double
      theEnergyOf par = do ps <- ask
                           case F.find (`is` par) ps of
                             Just p  -> return (energyOf p)
                             _       -> lift Nothing

data Choice = ByMin | ByMax
data HowPair = HowPair ([Particle] -> Maybe Double) Choice

pairByM :: EventEntry -> ParticlePairs
pairByM = pairBy (HowPair (Just . invariantMass) ByMin)

pairByPT :: EventEntry -> ParticlePairs
pairByPT = pairBy (HowPair (Just . ptVectorSum) ByMax)

pairByTheta :: EventEntry -> ParticlePairs
pairByTheta = pairBy (HowPair cosTh ByMax)

pairByR :: EventEntry -> ParticlePairs
pairByR = pairBy (HowPair dR ByMin)

pairBy :: HowPair -> EventEntry -> ParticlePairs
pairBy (HowPair func choice) pm =
    let allpairs = (map F.toList . particlesOfAllBL) pm
        pairMap = foldr (\p m -> M.insert (func p) p m) M.empty allpairs
        chosenPair = (\case ByMax -> M.maxView pairMap
                            _     -> M.minView pairMap) choice
    in case chosenPair of Just (pair, _) -> [S.fromList pair]
                          _              -> []

mBLTrue :: EventEntry -> ByteString
mBLTrue = headOf . runReader (calcVar 2 particlesFromTop (Just . invariantMass))

mBLAll :: EventEntry -> [ByteString]
mBLAll = runReader $ calcVar 2 particlesOfAllBL (Just . invariantMass)

pTTrue :: EventEntry -> ByteString
pTTrue = headOf . runReader (calcVar 2 particlesFromTop (Just . ptVectorSum))

pTAll :: EventEntry -> [ByteString]
pTAll = runReader $ calcVar 2 particlesOfAllBL (Just . ptVectorSum)

cosThetaTrue :: EventEntry -> ByteString
cosThetaTrue = headOf . runReader (calcVar 3 particlesFromTop (cosTh . F.toList))

cosThetaAll :: EventEntry -> [ByteString]
cosThetaAll = runReader $ calcVar 3 particlesOfAllBL (cosTh . F.toList)

deltaRTrue :: EventEntry -> ByteString
deltaRTrue = headOf . runReader (calcVar 3 particlesFromTop (dR . F.toList))

deltaRAll :: EventEntry -> [ByteString]
deltaRAll = runReader $ calcVar 3 particlesOfAllBL (dR . F.toList)

calcVar :: Int -> (EventEntry -> ParticlePairs) -> (Seq Particle -> Maybe Double)
           -> Reader EventEntry [ByteString]
calcVar n mkpair func = liftM (map (toFixed n) . runReader (calcVar' func)) ask
    where calcVar' :: (Seq Particle -> Maybe Double) -> Reader EventEntry [Double]
          calcVar' f = do
            pm' <- ask
            let pss = filter containsBL (mkpair pm')
            return $ mapMaybe f (F.toList pss)

missingET :: EventEntry -> ByteString
missingET = toFixed 2 . ptVectorSum . filter (`is` invisible) . runReader finalStates

headOf :: [ByteString] -> ByteString
headOf (x:_) = x
headOf _     = "-10"

cosTh :: [Particle] -> Maybe Double
cosTh [p,p'] = Just $ cosTheta p p'
cosTh _      = Nothing

dR :: [Particle] -> Maybe Double
dR [p,p'] = Just $ deltaR p p'
dR _      = Nothing
