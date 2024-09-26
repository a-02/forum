{-# LANGUAGE RecordWildCards #-}
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

calcSun :: Double -> Double -> UTCTime -> Sun
calcSun latitude longitude date = 
  let d :: Double = (toEnum . fromEnum $ utctDay date) - 51545.5
      lat = latitude * (pi/180)
      long = longitude * (pi/180)
      inRadians = (* (pi/180))
      -- denoted "g"
      meanAnomaly = inRadians $ (357.529 + (0.98560028 * d)) `mod'` 360.0 
      -- denoted "e"
      meanObliquityOfEcliptic = inRadians $ (23.439 - (0.00000036 * d)) `mod'` 360.0 -- in degrees
      -- denoted "q"
      meanLongitude = (280.459 + (0.98564736 * d)) `mod'` 360.0
      -- denoted "L"
      apparentElipticLongitude = inRadians $ (meanLongitude + (1.915 * sin meanAnomaly) + (0.020 * sin (2 * meanAnomaly)))
      gmst = 18.697375 + (24.065709824279 * d) `mod'` 24.0
      rightAscension = atan $ (cos meanObliquityOfEcliptic * sin apparentElipticLongitude) / cos apparentElipticLongitude
      declination = sin meanObliquityOfEcliptic / sin apparentElipticLongitude
      geometricAltitudeOfSun = -(5/6)
      intermediateValue = (* (pi/180)) $ mod' 180 $ (sin geometricAltitudeOfSun - (sin latitude * sin declination)) / (cos lat * cos declination) 
      transit = (rightAscension + long - gmst) / 360.0
      rising = transit - (intermediateValue / 360.0)
      setting = transit + (intermediateValue / 360.0)
   in Sun {..}

toSec :: Double -> ((Int -> r) -> r)
toSec timeFraction = \k -> k (round $ timeFraction * 86400)

hours :: Integral a => a -> ((a, a) -> t) -> t
hours x = \k -> k (x `quotRem` 3600)

minutes :: Integral a => a -> ((a, a) -> t) -> t
minutes m = \k -> k (m `quotRem` 60)

decToTime :: Double -> (((Int, Int, Int) -> r) -> r)
decToTime time = \k ->
  toSec time $ \secondsTotal ->
  hours secondsTotal $ \(h,m) ->
  minutes m $ \(m',s) ->
  k (h,m',s)