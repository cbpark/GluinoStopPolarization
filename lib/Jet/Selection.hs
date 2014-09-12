{-# LANGUAGE RecordWildCards #-}

module Jet.Selection where

import           Control.Monad              (liftM)
import           Control.Monad.Trans.Reader

import           Object.Particles

import           HEP.Data.LHEF

data PtEtaCut = PtEtaCut { ptcut :: Double, etacut :: Double }

selectJ :: Reader EventEntry [Particle]
selectJ = selectPars quarkJet (PtEtaCut 20 2.8)

selectB :: Reader EventEntry [Particle]
selectB = selectPars bQuark (PtEtaCut 20 2.8)

selectL :: Reader EventEntry [Particle]
selectL = selectPars lepton (PtEtaCut 20 2.5)

selectT :: Reader EventEntry [Particle]
selectT = selectPars tau (PtEtaCut 20 2.5)

selectPars :: ParticleType -> PtEtaCut -> Reader EventEntry [Particle]
selectPars ptype cut = liftM (filter (\p -> (p `is` ptype) &&
                                            ptEtaCutFor p cut)) finalStates
    where ptEtaCutFor p PtEtaCut { .. } = pt p > ptcut && (abs . eta) p < etacut

selectM :: Reader EventEntry Particle
selectM = liftM (head . filter (`is` invisible)) finalStates

finalObjs :: Reader EventEntry ParObjs
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
lepJetIsol jets leps | null jets = leps
                     | otherwise = filter (`isolated` jets) leps
                     where isolated l = all (\j -> deltaR l j > 0.4)
