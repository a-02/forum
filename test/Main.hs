{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Thyme
import Control.Lens
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Basic (testsNames)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All Tests" [astronomy]

-- NOTE: THE NOAA DOESN'T LABEL THEIR MAGIC NUMBERS
-- SO I WON'T EITHER

latitudeChi :: Double
latitudeChi = 41.85
longitudeChi :: Double
longitudeChi = -87.65
timeZone :: Double
timeZone = -6.0
testDate :: Double
testDate = 2454686.75 -- Aug. 8th, 2008, 6:00PM (local time)

toJulianDate :: UTCTime -> Double
toJulianDate (view utcTime -> UTCView day dt) =
  fromIntegral (toModifiedJulianDay day) + toSeconds dt / 86400 + 2400000 + (timeZone / 24)

testTime :: Double
testTime = mkUTCTime 2008 8 9 0 6 0 & toJulianDate

testCentury :: Double
testCentury = 0.08601654 -- (testDate - 24515445)/36525
-- testGeomMeanLongSun =
--   mod' 360 (280.46646 + testCentury * (36000.76983 + testCentury * 0.0003032))
testGeomMeanLongSun :: Double
testGeomMeanLongSun = 137.1281629 -- degrees
-- testGeomMeanAnomSun =
--   357.52911 + testCentury * (35999.05029-(0.0001537 * testCentury))
testGeomMeanAnomSun :: Double
testGeomMeanAnomSun = 3454.042901 -- degrees
-- testEccentEarthOrbit =
--   0.016708634 - (tC * (0.000042037 + 0.0000001267 * tC))
testEccentEarthOrbit :: Double
testEccentEarthOrbit = 0.0167051719
{- 
testSunEquationOfCenter =
  let g = testGeomMeanAnomSun
      tC = testCentury
   in (sin $ radians g) *
      (1.914602 - tC * (0.004817 + 0.000014 * tC)) +
      (sin (radians $ 2 * g) * (0.019993 - 0.000101 * tC)) +
      (sin (radians $ 3 * g)) *
      0.000289
-}
testSunEquationOfCenter :: Double
testSunEquationOfCenter = -1.053330252

-- testSunTrueLong = testGeomMeanLongSun + testSunEquationOfCenter
testSunTrueLong :: Double
testSunTrueLong = 136.0748327 -- degrees

-- testSunTrueAnom = testGeomMeanAnomSun + testSunEquationOfCenter
testSunTrueAnom :: Double
testSunTrueAnom = 3452.98957 -- degrees

{-
testSunRadVector = 
  let k = testEccentEarthOrbit
      n = testSunTrueAnom
   in (1.000001018 * (1 - k * k)) / (1 + k * cos (radians n))
-}
testSunRadVector :: Double
testSunRadVector = 1.013928788 -- AUs

{-
testSunAppLong =
  let m = testSunTrueLong
      g = testCentury
   in m - 0.00569 - 0.00478 * sin (radians $ 125.04 - 1934.136 * g)
-}
testSunAppLong :: Double
testSunAppLong = 136.0722992 -- degrees

{-
testMeanObliqEcliptic =
  23 + 
    (26 + 
      (21.448 - tC * 
        (46.815 + tC * 
          (0.00059 - tC * 0.001813)
        )
      )
    / 60) 
  / 60
-}
testMeanObliqEcliptic :: Double
testMeanObliqEcliptic = 23.43817254 -- degrees

{-
testObliqCorr = testMeanObliqEcliptic +
  0.00256 * (cos $ radians (125.04 - 1934.136 * tC))
-}
testObliqCorr :: Double
testObliqCorr = 23.44009496 -- degrees

{-
testSunRtAscension = degrees . atan2 $
  (cos $ radians testSunAppLong)
  ((cos $ radians testObliqCorr) * (sin $ radians testSunAppLong))
-}
testSunRtAscension :: Double
testSunRtAscension = 138.5309704 -- degrees

{-
testSunDeclin = degrees . asin $
  (sin $ radians testObliqCorr) * (sin $ radians testSunAppLong)
-}
testSunDeclin :: Double
testSunDeclin = 16.01964197 -- degress

-- testVarY = (tan $ radians (testObliqCorr / 2)) ^ 2
testVarY :: Double
testVarY = 0.04303756502

{-

oooookay

testEquationOfTime = 
  let u = testVarY
      i = testGeomMeanLongSun
      k = testEccentEarthOrbit
      j = testGeomMeanAnomSun
   in (*4) . degrees $
        (u * sin $ 2 * (radians i)) -
        (2 * k * sin $ radians j) +
        (4 * k * u * (sin $ radians j) * (cos $ 2 * radians i)) -
        (0.5 * (u ^ 2) * (sin $ 4 * radians i)) -
        (1.25 * (k ^ 2) * (sin $ 2 * radians j))

-}
testEquationOfTime :: Double
testEquationOfTime = -5.619911532 -- minutes

{-
testHASunrise = degrees . acos $
  (cos $ radians 90.833) / (cos $ radians latitudeChi) * (cos $ radians testSunDeclin) - (tan $ radians latitudeChi) * (tan $ radians testSunDeclin)
-}
testHASunrise :: Double
testHASunrise = 106.10917 -- degrees

{-
testSolarNoon = (720 - 4*longitudeChi - testEquationOfTime + timeZone)
-}
testSolarNoon :: Double
testSolarNoon = 0.4973749386 -- fraction of day. 11:56:13

-- testSunrise = testSolarNoon - testHASunrise * 4 / 1440
testSunrise :: Double
testSunrise = 0.2026272411 -- fraction of day, 04:51:47

-- testSunset = testSolarNoon + testHASunrise * 4 / 1440
testSunset :: Double
testSunset = 0.7921226331 -- fraction of day, 19:00:39

astronomy :: TestTree
astronomy = testGroup "Astronomy Tests"
  [ testCase "dummy test" $ True @? "How did you do this?"]