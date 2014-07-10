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
import           Data.Maybe                        (fromMaybe)

type JetLevelResult = Map.Map C.ByteString ByteString

calcVar :: Reader ParticleMap JetLevelResult
calcVar = do
  fobj <- finalObjs
  let alljet = (++) <$> jet <*> bjet $ fobj
      (nl, nb, ntau) = (,,) <$>
                       length . isoLep <*> length . bjet <*> length . taujet $
                       fobj
      nj = length . filter (\p -> transMomentumOne p > 30) $ alljet
      met = transMomentumOne (missingPt fobj)
      hT = hTinc fobj
      meff = met + hT
      mT = fromMaybe (-1) (transMassLep fobj)
      pTj1 = if null alljet then -1 else maximum (map transMomentumOne alljet)
  return $ Map.fromList [ ("nl",   (pack . show) nl)
                        , ("nb",   (pack . show) nb)
                        , ("ntau", (pack . show) ntau)
                        , ("nj",   (pack . show) nj)
                        , ("met",  toFixed 2 met)
                        , ("HT",   toFixed 2 hT)
                        , ("meff", toFixed 2 meff)
                        , ("mT",   toFixed 2 mT)
                        , ("pTj1", toFixed 2 pTj1)
                        ]

hTinc :: ParObjs -> Double
hTinc ParObjs { .. } =
    sum (filter (>30) (map transMomentumOne (jet ++ bjet))) +
            (if null isoLep then 0 else (transMomentumOne . head) isoLep)

transMassLep :: ParObjs -> Maybe Double
transMassLep ParObjs { .. }
    | null isoLep = Nothing
    | otherwise   = Just $ transMassOne (head isoLep) missingPt
