{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

module Parton.ParSelector
    (
      ParticlePairs

    , topQuark
    , bQuark
    , lepton
    , invisible
    , particlesFromTop
    , particlesOfAllBL
    , containsBL

    , cosTheta
    , dR
    , transMomentum
    ) where

import           HEP.Data.LHEF

import           Control.Applicative
import           Data.Maybe          (fromMaybe)

type ParticlePairs = [[Particle]]

topQuark :: ParType
topQuark = [6]

bQuark :: ParType
bQuark = [5]

lepton :: ParType
lepton = [11,13]

invisible :: ParType
invisible = [12,14,1000024]

data ParSelec = ParSelec { ptype  :: ParType
                         , ptcut  :: Double
                         , etacut :: Double }

selectB :: ParSelec
selectB = ParSelec { ptype = bQuark, ptcut = 30, etacut = 2.5 }

selectL :: ParSelec
selectL = ParSelec { ptype = lepton, ptcut = 25, etacut = 2.4 }

basicCutFor :: ParSelec -> Particle -> Bool
basicCutFor ParSelec {..} p
    | p `is` ptype = transMomentum [p] > ptcut && (abs . rapidity) p < etacut
    | otherwise    = False

particlesFromTop :: ParticleMap -> ParticlePairs
particlesFromTop pm =
    let ps = particlesFrom topQuark pm
    in map (filter ((||) <$> basicCutFor selectB <*> basicCutFor selectL)) ps

particlesOfAllBL :: ParticleMap -> ParticlePairs
particlesOfAllBL pm = let !fstates = finalStates pm
                          !leps = filter (basicCutFor selectL) fstates
                          !bs = filter (basicCutFor selectB) fstates
                      in if (length leps /= 1) || (length bs < 3)
                         then []
                         else [[lep,b] | lep <- leps, b <- bs,
                                         invMass [lep,b] < 160 && fromMaybe (-1) (dR [lep,b]) > 0.4]

containsBL :: [Particle] -> Bool
containsBL ps = let (totnb, totnl) = counter ps
                in (totnb == 1) && (totnl == 1)
    where counter :: [Particle] -> (Int, Int)
          counter = foldr (\p (nb, nl) ->
                               let pid = abs (idOf p)
                               in if | pid `elem` bQuark -> (nb+1, nl  )
                                     | pid `elem` lepton -> (nb  , nl+1)
                                     | otherwise         -> (nb  , nl  )) (0, 0)
