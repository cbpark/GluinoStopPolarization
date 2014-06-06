module Calculation.Variables
    (
    -- * Energy ratio of b quark and lepton
      eRatioBLTruePair
    ) where

import           HEP.Data.LHEF

import           Calculation.ParSelector

import           Control.Monad                     (filterM)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Reader        (Reader, ReaderT (..), ask,
                                                    runReader)
import qualified Data.ByteString.Char8             as B
import           Data.Double.Conversion.ByteString (toFixed)

eRatioBLTruePair :: ParticleMap -> B.ByteString
eRatioBLTruePair = runReader $ eRatioBLpair particlesFromTop

eRatioBLpair :: (ParticleMap -> [[Particle]]) -> Reader ParticleMap B.ByteString
eRatioBLpair getPair = do pm <- ask
                          let e = case eRatioBL (getPair pm) of Nothing -> 0
                                                                Just [] -> 0
                                                                Just (r:_) -> r
                          return $ toFixed 3 e

eRatioBL :: [[Particle]] -> Maybe [Double]
eRatioBL pss =
    -- The elements of blpairs are guaranteed to have exactly one pair of
    -- b-quark and lepton or nothing by containsBL.
    mapM (runReaderT eRatioBL') =<< filterM containsBL pss

eRatioBL' :: ReaderT [Particle] Maybe Double
eRatioBL' = do eLepton <- theEnergy lepton
               eBquark <- theEnergy bQuark
               return $ eLepton / (eBquark + eLepton)
    where theEnergy :: [Int] -> ReaderT [Particle] Maybe Double
          theEnergy ns = do ps <- ask
                            case filter (inParticles ns) ps of
                              []    -> lift Nothing
                              (p:_) -> return (energyOf p)
