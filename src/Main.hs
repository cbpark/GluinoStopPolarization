{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           HEP.Data.LHEF                   (Particle (..), ParticleMap,
                                                  energyOf, getDaughters, idOf,
                                                  inParticles, particleLineOf)
import           HEP.Data.LHEF.Parser            (parseEvent, stripLHEF)

import           Control.Monad                   (join, when)
import           Control.Monad.Trans.Class       (lift)
import           Control.Monad.Trans.Reader      (ReaderT (..), ask)
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import qualified Data.ByteString.Lazy.Char8      as C
import           Data.Maybe                      (fromMaybe)
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure)

parseAndCalc :: C.ByteString -> IO ()
parseAndCalc str = case parse parseEvent str of
                     Fail r _ _               -> C.putStr r
                     Done evRemained evParsed ->
                         do let parmap = snd evParsed
                            print $ eRatioBL (particlesFromTop parmap)
                            -- print $ particlesFromTop parmap
                            parseAndCalc evRemained

topQuark :: [Int]
topQuark = [6]

bQuark :: [Int]
bQuark = [5]

lepton :: [Int]
lepton = [11,13]

particlesFromTop :: ParticleMap -> [[Particle]]
particlesFromTop pm =
    let tops = particleLineOf topQuark pm
    in map (filter (inParticles (bQuark ++ lepton)) . getDaughters pm) tops

eRatioBL :: [[Particle]] -> Double
eRatioBL pss =
    let blpairs = filter containsBL pss
        result = (join . safeHead . map (runReaderT eRatioBL')) blpairs
    in fromMaybe 0 result

eRatioBL' :: ReaderT [Particle] Maybe Double
eRatioBL' = do eLepton <- theEnergy lepton
               eBquark <- theEnergy bQuark
               return $ eLepton / (eBquark + eLepton)

theEnergy :: [Int] -> ReaderT [Particle] Maybe Double
theEnergy ns = do ps <- ask
                  case (safeHead . filter (inParticles ns)) ps of
                    Just p  -> return (energyOf p)
                    Nothing -> lift Nothing

containsBL :: [Particle] -> Bool
containsBL ps = let (totnb, totnl) = counter ps
                in (totnb == 1) && (totnl == 1)
    where
      counter :: [Particle] -> (Int, Int)
      counter = foldr (\p (nb, nl) ->
                           let pid = abs (idOf p)
                           in if | pid `elem` bQuark -> (nb+1, nl  )
                                 | pid `elem` lepton -> (nb  , nl+1)
                                 | otherwise         -> (nb  , nl  )) (0, 0)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

main :: IO ()
main = do
  args <- getArgs

  when (length args /= 1) $ do
         putStrLn "Supply the input file name."
         exitFailure

  evstr <- C.readFile (head args)
  (parseAndCalc . stripLHEF) evstr
