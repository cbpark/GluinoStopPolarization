{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE RecordWildCards #-}

module Calculation.ParSelector where

import           HEP.Data.LHEF
import           HEP.Vector.LorentzVector

import           Control.Applicative

type ParticlePairs = [[Particle]]

topQuark :: ParType
topQuark = [6]

bQuark :: ParType
bQuark = [5]

lepton :: ParType
lepton = [11,13]

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
particlesOfAllBL pm = let fstates = finalStates pm
                          lep = filter (basicCutFor selectL) fstates
                          bs = filter (basicCutFor selectB) fstates
                      in if null lep || null bs
                         then []
                         else foldr (\b xs -> ((b:lep):xs)) [] bs

containsBL :: [Particle] -> Maybe Bool
containsBL ps = do let (totnb, totnl) = counter ps
                   return $ (totnb == 1) && (totnl == 1)
    where counter :: [Particle] -> (Int, Int)
          counter = foldr (\p (nb, nl) ->
                               let pid = abs (idOf p)
                               in if | pid `elem` bQuark -> (nb+1, nl  )
                                     | pid `elem` lepton -> (nb  , nl+1)
                                     | otherwise         -> (nb  , nl  )) (0, 0)
