{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Jet.Variables where

import           Control.Applicative
import           Control.Monad.Trans.Reader
import           Data.ByteString.Char8             (ByteString, pack)
import qualified Data.ByteString.Lazy.Char8        as C
import           Data.Double.Conversion.ByteString (toFixed)
import           Data.Map                          (Map, fromList, maxView)
import           Data.Maybe                        (fromMaybe)

import           HEP.Data.LHEF

import           Jet.Selection                     (finalObjs)
import           Object.Particles                  (ParObjs (..))

type JetLevelResult = Map C.ByteString ByteString

calcVar :: Reader EventEntry JetLevelResult
calcVar = do
  !fobj <- finalObjs
  let !alljet = (++) <$> jet <*> bjet $ fobj
      (nl, nb, ntau) = (,,) <$>
                       length . isoLep <*> length . bjet <*> length . taujet $
                       fobj
      nj = length . filter (\p -> pt p > 30) $ alljet
      met = pt (missingPt fobj)
      hT = hTinc fobj
      meff = met + hT
      mT = fromMaybe (-1) (transMassLep fobj)
      pTj1 = if null alljet then -1 else maximum (map pt alljet)
      mbl_by_theta = mBL fobj
      er_by_theta = eRatioBL fobj
  return $ fromList [ ("nl",          (pack . show) nl)
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
    (sum . filter (>30) . map pt) (jet ++ bjet) +
    (sum . filter (>20) . map pt) isoLep

transMassLep :: ParObjs -> Maybe Double
transMassLep ParObjs { .. }
    | null isoLep = Nothing
    | otherwise   = Just $ transverseMass (head isoLep) (setXYM kx ky 0)
  where kx = let (x, _, _, _, _) = pup missingPt in x
        ky = let (_, y, _, _, _) = pup missingPt in y

mBL :: ParObjs -> Double
mBL ParObjs { .. } =
    case (maxView . fromList) [(cosTheta b l, invariantMass [b,l]) |
                               b <- bjet, l <- isoLep] of
      Just (mbl, _) -> mbl
      Nothing       -> -1

eRatioBL :: ParObjs -> Double
eRatioBL ParObjs { .. } =
    case (maxView . fromList) [(cosTheta b l,
                                let (eB, eL) = (energyOf b, energyOf l)
                                in eL / (eB + eL))
                               | b <- bjet, l <- isoLep, invariantMass [b,l] < 165] of
      Just (er, _) -> er
      _            -> -1
