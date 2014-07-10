module Object.Particles
    (
      ParObjs (..)
    , ParticlePairs

    , topQuark
    , bQuark
    , quarkJet
    , lepton
    , tau
    , invisible
    ) where

import           HEP.Data.LHEF

data ParObjs = ParObjs { isoLep    :: [Particle]
                       , taujet    :: [Particle]
                       , jet       :: [Particle]
                       , bjet      :: [Particle]
                       , missingPt :: Particle
                       }

type ParticlePairs = [[Particle]]

topQuark :: ParticleType
topQuark = ParticleType [6]

bQuark :: ParticleType
bQuark = ParticleType [5]

quarkJet :: ParticleType
quarkJet = ParticleType [21]

lepton :: ParticleType
lepton = ParticleType [11,13]

tau :: ParticleType
tau = ParticleType [15]

invisible :: ParticleType
invisible = ParticleType [12,14,1000024]
