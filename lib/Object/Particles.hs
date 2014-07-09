module Object.Particles
    (
      ParticlePairs

    , topQuark
    , bQuark
    , lepton
    , invisible
    ) where

import           HEP.Data.LHEF

type ParticlePairs = [[Particle]]

topQuark :: ParticleType
topQuark = ParticleType [6]

bQuark :: ParticleType
bQuark = ParticleType [5]

lepton :: ParticleType
lepton = ParticleType [11,13]

invisible :: ParticleType
invisible = ParticleType [12,14,1000024]
