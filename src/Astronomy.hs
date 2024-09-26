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

calcSun' :: Double -> Double -> UTCTime -> IO ()
calcSun' latitude longitude date = do
  -- step 1. calculate the current julian day
  let day = toEnum . fromEnum $ utctDay date
      j2000day = day - 51545.0
      degToRadians :: Double -> Double
      degToRadians = (* (pi/180))
      radToDegrees :: Double -> Double
      radToDegrees = (* (180/pi))
  -- step 2. calculate the mean solar time
  let meanSolarTime = j2000day - longitude/360.0
  -- step 3. mean solar anomaly, in degrees
  let meanSolarAnomaly = mod' (357.5291 + 0.9850028 * j2000day) 360.0
  -- step 4. equation of the center, in degrees
  let m = meanSolarAnomaly
      m' = degToRadians meanSolarAnomaly
      equationOfTheCenter = radToDegrees $ 1.9148 * sin m' + 0.02 * sin (2 * m') + 0.00003 * sin (3 * m')
  -- step 5. ecliptic longitude
  let c = equationOfTheCenter
      eclipticLongitude = mod' (m + c + 282.9372) 360.0
  -- step 6. transit
  let el = eclipticLongitude
      el' = degToRadians eclipticLongitude
      transit = 2451545 + j2000day + radToDegrees (0.0053 * sin m') - radToDegrees (0.0069 * sin (2 * el'))
  -- step 7. declination
  let declination = radToDegrees . asin $ sin el' * sin (degToRadians 23.4397)
      delta = degToRadians declination
  -- step 8. hour angle
  let lat = degToRadians latitude
      hourAngleNum = sin (degToRadians (- (5/6))) - (sin lat * sin delta)
      hourAngleDen = cos lat * cos delta
      hourAngleIntermediate = hourAngleNum / hourAngleDen
      hourAngle = radToDegrees . acos $ hourAngleNum / hourAngleDen
  -- step 9. sunrise & sunset
  let rise = transit - (hourAngle / 360.0)
      set = transit + (hourAngle / 360.0)
  decToTime rise print
  decToTime set print

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