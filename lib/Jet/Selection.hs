{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE RecordWildCards #-}

module Jet.Selection where

import           Object.Particles

import           HEP.Data.LHEF

import           Control.Monad              (liftM)
import           Control.Monad.Trans.Reader
import           Data.Maybe                 (fromJust)

data PtEtaCut = PtEtaCut { ptcut :: Double, etacut :: Double }

selectJ :: Reader ParticleMap [Particle]
selectJ = selectPars quarkJet (PtEtaCut 20 2.8)

selectB :: Reader ParticleMap [Particle]
selectB = selectPars bQuark (PtEtaCut 20 2.8)

selectL :: Reader ParticleMap [Particle]
selectL = selectPars lepton (PtEtaCut 20 2.5)

selectT :: Reader ParticleMap [Particle]
selectT = selectPars tau (PtEtaCut 20 2.5)

selectPars :: ParticleType -> PtEtaCut -> Reader ParticleMap [Particle]
selectPars ptype cut = liftM (filter (\p -> (p `is` ptype) &&
                                            ptEtaCutFor p cut)) finalStates
    where ptEtaCutFor p PtEtaCut { .. } =
              transMomentumOne p > ptcut && (abs . rapidity) p < etacut

selectM :: Reader ParticleMap Particle
selectM = liftM (head . filter (`is` invisible)) finalStates

finalObjs :: Reader ParticleMap ParObjs
finalObjs = do
  jets  <- selectJ
  bjets <- selectB
  leps  <- liftM (lepJetIsol $ jets ++ bjets) selectL
  taus  <- selectT
  mpt   <- selectM
  return ParObjs { isoLep    = leps
                 , taujet    = taus
                 , jet       = jets
                 , bjet      = bjets
                 , missingPt = mpt
                 }

-- | Resolves overlaps between jets and leptons.
lepJetIsol :: [Particle] -- ^ Jets
           -> [Particle] -- ^ Lepton candidates
           -> [Particle]
lepJetIsol !jets !leps | null jets = leps
                       | otherwise = filter (`isolated` jets) leps
                       where isolated l = all (\j -> fromJust (dR [l,j]) > 0.4)
