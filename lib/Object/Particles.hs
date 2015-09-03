module Object.Particles
    (
      ParObjs (..)
    , ParticlePairs

    , topSquark
    , topQuark
    , bQuark
    , quarkJet
    , lepton
    , tau
    , invisible
    ) where

import           HEP.Data.LHEF hiding (tau)

data ParObjs = ParObjs { isoLep    :: [Particle]
                       , taujet    :: [Particle]
                       , jet       :: [Particle]
                       , bjet      :: [Particle]
                       , missingPt :: Particle
                       }

type ParticlePairs = [[Particle]]

topSquark :: ParticleType
topSquark = ParticleType [1000006]

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
invisible = ParticleType [12,14,1000022,1000024]
