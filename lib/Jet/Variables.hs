{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Jet.Variables where

import           Jet.Selection                     (finalObjs)
import           Object.Particles                  (ParObjs (..))

import           HEP.Data.LHEF

import           Control.Applicative
import           Control.Monad.Trans.Reader
import           Data.ByteString.Char8             (ByteString, pack)
import qualified Data.ByteString.Lazy.Char8        as C
import           Data.Double.Conversion.ByteString (toFixed)
import qualified Data.Map                          as Map
import           Data.Maybe                        (fromJust, fromMaybe)

type JetLevelResult = Map.Map C.ByteString ByteString

calcVar :: Reader ParticleMap JetLevelResult
calcVar = do
  !fobj <- finalObjs
  let !alljet = (++) <$> jet <*> bjet $ fobj
      (nl, nb, ntau) = (,,) <$>
                       length . isoLep <*> length . bjet <*> length . taujet $
                       fobj
      nj = length . filter (\p -> transMomentumOne p > 30) $ alljet
      met = transMomentumOne (missingPt fobj)
      hT = hTinc fobj
      meff = met + hT
      mT = fromMaybe (-1) (transMassLep fobj)
      pTj1 = if null alljet then -1 else maximum (map transMomentumOne alljet)
      mbl_by_theta = mBL fobj
      er_by_theta = eRatioBL fobj
  return $ Map.fromList [ ("nl",          (pack . show) nl)
                        , ("nb",          (pack . show) nb)
                        , ("ntau",        (pack . show) ntau)
                        , ("nj",          (pack . show) nj)
                        , ("met",         toFixed 2 met)
                        , ("HT",          toFixed 2 hT)
                        , ("meff",        toFixed 2 meff)
                        , ("mT",          toFixed 2 mT)
                        , ("pTj1",        toFixed 2 pTj1)
                        , ("m_bl_theta",  toFixed 2 mbl_by_theta)
                        , ("er_by_theta", toFixed 3 er_by_theta)
                        ]

hTinc :: ParObjs -> Double
hTinc ParObjs { .. } =
    sum (filter (>30) (map transMomentumOne (jet ++ bjet))) +
            (if null isoLep then 0 else (transMomentumOne . head) isoLep)

transMassLep :: ParObjs -> Maybe Double
transMassLep ParObjs { .. }
    | null isoLep = Nothing
    | otherwise   = Just $ transMassOne (head isoLep) missingPt

mBL :: ParObjs -> Double
mBL ParObjs { .. } =
    case (Map.maxView . Map.fromList) [(fromJust (cosTheta [b,l]), invMass [b,l])
                                       | b <- bjet, l <- isoLep] of
      Just (mbl, _) -> mbl
      Nothing       -> -1

eRatioBL :: ParObjs -> Double
eRatioBL ParObjs { .. } =
    case (Map.maxView . Map.fromList)
             [(fromJust (cosTheta [b,l]),
               let (eB, eL) = (energyOf b, energyOf l) in eL / (eB + eL))
              | b <- bjet, l <- isoLep, invMass [b,l] < 165] of
      Just (er, _) -> er
      _            -> -1
