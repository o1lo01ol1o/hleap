{-|
Module      :  System.Hardware.Leap.Event.Types
Copyright   :  (c) 2016 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <consult@brianwbush.info>
Stability   :  Stable
Portability :  Portable


Types for Leap Motion \<<https://www.leapmotion.com/product/desktop>\>, based on the Web Socket API \<<https://developer.leapmotion.com/documentation/javascript/supplements/Leap_JSON.html>\>.
-}


module System.Hardware.Leap.Types (
-- * Types
  LeapId
, Duration
, Vector
, Basis
, Matrix
) where


-- | ID for an item tracked by Leap.
type LeapId = Int


-- | Measurement of time.
type Duration = Int


-- | Three dimensional vector.
type Vector a = (a, a, a)


-- | Three dimensionsional basis.
type Basis a = (Vector a, Vector a, Vector a)


-- | Three dimensionsional rotation matrix.
type Matrix a = (Vector a, Vector a, Vector a)
