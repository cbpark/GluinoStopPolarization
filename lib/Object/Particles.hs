module Object.Particles
    (
      FourMomentum
    , ParObjs (..)
    , ParticlePairs

    , topQuark
    , bQuark
    , quarkJet
    , lepton
    , invisible
    ) where

import           HEP.Data.LHEF

data ParObjs = ParObjs { isoLep    :: [FourMomentum]
                       , jet       :: [FourMomentum]
                       , bjet      :: [FourMomentum]
                       , missingPt :: TwoMomentum
                       } deriving Show

type ParticlePairs = [[Particle]]

topQuark :: ParticleType
topQuark = ParticleType [6]

bQuark :: ParticleType
bQuark = ParticleType [5]

quarkJet :: ParticleType
quarkJet = ParticleType [21]

lepton :: ParticleType
lepton = ParticleType [11,13]

invisible :: ParticleType
invisible = ParticleType [12,14,1000024]
