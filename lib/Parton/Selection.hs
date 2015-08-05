{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

module Parton.Selection
    (
      particlesFromTop
    , particlesOfAllBL
    , containsBL
    ) where

import           Control.Monad.Trans.Reader
import qualified Data.Foldable              as F

import           HEP.Data.LHEF

import           Object.Particles

data ParSelec = ParSelec { ptype  :: ParticleType
                         , ptcut  :: Double
                         , etacut :: Double }

selectB :: ParSelec
selectB = ParSelec { ptype = bQuark, ptcut = 30, etacut = 2.5 }

selectL :: ParSelec
selectL = ParSelec { ptype = lepton, ptcut = 25, etacut = 2.4 }

basicCutFor :: ParSelec -> Particle -> Bool
basicCutFor ParSelec {..} p
    | p `is` ptype = pt p > ptcut && (abs . eta) p < etacut
    | otherwise    = False

particlesFromTop :: EventEntry -> ParticlePairs
particlesFromTop pm =
    let ps = runReader (particlesFrom topQuark) pm
    in map (filter ((||) <$> basicCutFor selectB <*> basicCutFor selectL)) ps

particlesOfAllBL :: EventEntry -> ParticlePairs
particlesOfAllBL pm = let fstates = runReader finalStates pm
                          leps = filter (basicCutFor selectL) fstates
                          bs = filter (basicCutFor selectB) fstates
                      in if null leps || null bs
                         then []
                         else [[lep,b] | lep <- leps, b <- bs,
                               invariantMass [lep, b] < 165 &&
                               deltaR lep b > 0.4 ]

containsBL :: [Particle] -> Bool
containsBL ps = let (totnb, totnl) = counter ps
                in (totnb == 1) && (totnl == 1)
    where counter :: [Particle] -> (Int, Int)
          counter = F.foldr (\p (nb, nl) -> if | p `is` bQuark -> (nb+1, nl  )
                                               | p `is` lepton -> (nb  , nl+1)
                                               | otherwise     -> (nb  , nl  ))
                    (0, 0)
