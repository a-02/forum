{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
module Astronomy where

import Data.Time
import Data.Fixed
import Data.Ratio

data Sun = Sun {
  meanObliquityOfEcliptic :: Double,
  meanAnomaly :: Double,
  meanLongitude :: Double,
  apparentElipticLongitude :: Double,
  gmst :: Double,
  rightAscension :: Double,
  declination :: Double,
  intermediateValue :: Double,
  transit :: Double,
  rising :: Double,
  setting :: Double
} deriving (Show)


toSec :: Double -> ((Int -> r) -> r)
toSec timeFraction k = k (round $ snd (properFraction timeFraction) * 86400)

hours :: Integral a => a -> ((a, a) -> t) -> t
hours x k = k (x `quotRem` 3600)

minutes :: Integral a => a -> ((a, a) -> t) -> t
minutes m k = k (m `quotRem` 60)

decToTime :: Double -> (((Int, Int, Int) -> r) -> r)
decToTime time k = toSec time $ \secondsTotal ->
  hours secondsTotal $ \(h,m) ->
  minutes m $ \(m',s) ->
  k (h,m',s)