{-# LANGUAGE RecordWildCards #-}

module Jet.Selection where

import           Object.Particles

import           HEP.Data.LHEF
import           HEP.Vector.LorentzVector   (deltaR)

import           Control.Monad              (liftM)
import           Control.Monad.Trans.Reader

data PtEtaCut = PtEtaCut { ptcut :: Double, etacut :: Double }

selectJ :: Reader ParticleMap [FourMomentum]
selectJ = selectPars quarkJet (PtEtaCut 20 2.5)

selectB :: Reader ParticleMap [FourMomentum]
selectB = selectPars bQuark (PtEtaCut 30 2.5)

selectL :: Reader ParticleMap [FourMomentum]
selectL = selectPars lepton (PtEtaCut 25 2.4)

selectPars :: ParticleType -> PtEtaCut -> Reader ParticleMap [FourMomentum]
selectPars ptype cut = liftM (map fourMomentum .
                                  filter (\p -> (p `is` ptype) &&
                                                ptEtaCutFor p cut)) finalStates
    where ptEtaCutFor p PtEtaCut { .. } =
              transMomentum [p] > ptcut && (abs . rapidity) p < etacut

selectM :: Reader ParticleMap Double
selectM = liftM (transMomentum . filter (`is` invisible)) finalStates

finalObjs :: Reader ParticleMap ParObjs
finalObjs = do
  jets  <- selectJ
  bjets <- selectB
  leps  <- liftM (lepJetIsol (jets ++ bjets)) selectL
  met   <- selectM
  return ParObjs { isoLep = leps
                 , jet    = jets
                 , bjet   = bjets
                 , met    = met
                 }

-- | Resolves overlaps between jets and leptons.
lepJetIsol :: [FourMomentum] -- ^Jets
           -> [FourMomentum] -- ^Lepton candidates
           -> [FourMomentum]
lepJetIsol jets = filter (`isolated` jets)
    where isolated k = all (\p -> p `deltaR` k > 0.4)
