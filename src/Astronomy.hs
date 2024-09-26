{-# LANGUAGE RecordWildCards #-}
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
  transit :: Double,
  rising :: Double,
  setting :: Double
}

calcSun :: Double -> Double -> UniversalTime -> Sun
calcSun latitude longitude date = 
  let d = fromRational $ getModJulianDate date - 2451545.0
      meanAnomaly = 357.529 + (0.98560028 * d) `mod'` 360.0 
      meanObliquityOfEcliptic = 23.439 - (0.00000036 * d) `mod'` 360.0 -- in degrees
      meanLongitude = 280.459 + (0.98564736 * d) `mod'` 360.0
      apparentElipticLongitude = meanLongitude + (1.915 * sin meanAnomaly) + (0.020 * sin (2 * meanAnomaly)) `mod'` 360.0
      gmst = 18.697375 + (24.065709824279 * d) `mod'` 24.0
      rightAscension = atan $ (cos meanObliquityOfEcliptic * sin apparentElipticLongitude) / cos apparentElipticLongitude
      declination = asin $ sin meanObliquityOfEcliptic / sin apparentElipticLongitude
      geometricAltitudeOfSun = -(5/6)
      intermediateValue = mod' 180 $ (sin geometricAltitudeOfSun - (sin latitude * sin declination)) / (cos latitude * cos declination) 
      transit = (rightAscension + longitude - gmst) / 360.0
      rising = transit - (intermediateValue / 360.0)
      setting = transit + (intermediateValue / 360.0)
   in Sun {..}