module Jet.Variables where

import           HEP.Data.LHEF

import           Control.Monad.Trans.Reader
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map                   as Map

type JetLevelResult = Map.Map C.ByteString ByteString

calcVar :: Reader ParticleMap JetLevelResult
calcVar = undefined
