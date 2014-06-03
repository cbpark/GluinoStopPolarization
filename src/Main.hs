{-# LANGUAGE OverloadedStrings #-}

module Main where

import           HEP.Data.LHEF                   (Particle (..), ParticleMap,
                                                  getDaughters, particleLineOf)
import           HEP.Data.LHEF.Parser            (parseEvent, stripLHEF)

import           Control.Monad                   (when)
import           Data.Attoparsec.ByteString.Lazy (Result (..), parse)
import qualified Data.ByteString.Lazy.Char8      as C
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure)

parseAndCalc :: C.ByteString -> IO ()
parseAndCalc str = case parse parseEvent str of
                     Fail r _ _               -> C.putStr r
                     Done evRemained evParsed ->
                         do let parmap = snd evParsed
                            print $ particleFromTop parmap
                            -- print parmap
                            parseAndCalc evRemained

particleFromTop :: ParticleMap -> [[Particle]]
particleFromTop pm =
    let tops = particleLineOf [6] pm
    in map (filter (inParticles [5, 11, 13]) . getDaughters pm) tops

inParticles :: [Int] -> Particle -> Bool
inParticles ns = (`elem` ns) . abs . idup

main :: IO ()
main = do
  args <- getArgs

  when (length args /= 1) $ do
         putStrLn "Supply the input file name."
         exitFailure

  str <- C.readFile (head args)
  (parseAndCalc . stripLHEF) str
