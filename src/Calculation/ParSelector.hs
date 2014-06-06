{-# LANGUAGE MultiWayIf #-}

module Calculation.ParSelector where

import           HEP.Data.LHEF

topQuark :: [Int]
topQuark = [6]

bQuark :: [Int]
bQuark = [5]

lepton :: [Int]
lepton = [11,13]

particlesFromTop :: ParticleMap -> [[Particle]]
particlesFromTop pm = let ps = particlesFrom topQuark pm
                      in map (filter (inParticles (bQuark ++ lepton))) ps

particlesOfAllBL :: ParticleMap -> [[Particle]]
particlesOfAllBL pm = let fstates = finalStates pm
                          lep = filter (inParticles lepton) fstates
                          bs = filter (inParticles bQuark) fstates
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
