{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

module Calculation.ParSelector
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
    , invMass
    , transMomentum
    ) where

import           HEP.Data.LHEF
import           HEP.Vector
import           HEP.Vector.LorentzVector

import           Control.Applicative
import           Data.Function            (on)

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
selectB = ParSelec { ptype = bQuark, ptcut = 0, etacut = 10 }

selectL :: ParSelec
selectL = ParSelec { ptype = lepton, ptcut = 0, etacut = 10 }

basicCutFor :: ParSelec -> Particle -> Bool
basicCutFor ParSelec {..} p
    | p `is` ptype = pT momentum > ptcut && (abs . eta) momentum < etacut
    | otherwise    = False
    where momentum = fourMomentum p

particlesFromTop :: ParticleMap -> ParticlePairs
particlesFromTop pm =
    let ps = particlesFrom topQuark pm
    in map (filter ((||) <$> basicCutFor selectB <*> basicCutFor selectL)) ps

particlesOfAllBL :: ParticleMap -> ParticlePairs
particlesOfAllBL pm = let !fstates = finalStates pm
                          !lep = filter (basicCutFor selectL) fstates
                          !bs = filter (basicCutFor selectB) fstates
                      in if null lep || null bs
                         then []
                         else foldr (\x xs -> if invMass (x:lep) < 160
                                              then (x:lep):xs
                                              else xs) [] bs

containsBL :: [Particle] -> Maybe Bool
containsBL ps = do let (totnb, totnl) = counter ps
                   return $ (totnb == 1) && (totnl == 1)
    where counter :: [Particle] -> (Int, Int)
          counter = foldr (\p (nb, nl) ->
                               let pid = abs (idOf p)
                               in if | pid `elem` bQuark -> (nb+1, nl  )
                                     | pid `elem` lepton -> (nb  , nl+1)
                                     | otherwise         -> (nb  , nl  )) (0, 0)

cosTheta :: [Particle] -> Double
cosTheta [p, p'] = cos $ (deltaTheta `on` fourMomentum) p p'
cosTheta _       = -10

dR :: [Particle] -> Double
dR [p, p'] = (deltaR `on` fourMomentum) p p'
dR _       = -10

invMass :: [Particle] -> Double
invMass = invariantMass . foldr ((.+.) . fourMomentum) zero

transMomentum :: [Particle] -> Double
transMomentum = pT . foldr ((.+.) . fourMomentum) zero
