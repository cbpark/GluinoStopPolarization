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
particlesFromTop pm =
    map (filter (inParticles (bQuark ++ lepton))) (particlesFrom topQuark pm)

containsBL :: [Particle] -> Maybe Bool
containsBL ps = do let (totnb, totnl) = counter ps
                   return $ (totnb == 1) && (totnl == 1)
    where counter :: [Particle] -> (Int, Int)
          counter = foldr (\p (nb, nl) ->
                               let pid = abs (idOf p)
                               in if | pid `elem` bQuark -> (nb+1, nl  )
                                     | pid `elem` lepton -> (nb  , nl+1)
                                     | otherwise         -> (nb  , nl  )) (0, 0)
