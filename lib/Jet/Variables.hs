module Jet.Variables where

import           HEP.Data.LHEF

import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map                   as Map

varJet :: Map.Map C.ByteString (ParticleMap -> ByteString)
varJet = undefined
