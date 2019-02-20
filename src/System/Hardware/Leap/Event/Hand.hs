{-|
Module      :  $Header$
Copyright   :  (c) 2016-19 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <code@functionally.io>
Stability   :  Stable
Portability :  Portable


Hand events for Leap Motion \<<https://www.leapmotion.com/product/desktop>\>, based on the Web Socket API \<<https://developer.leapmotion.com/documentation/javascript/supplements/Leap_JSON.html>\>.
-}


{-# LANGUAGE OverloadedStrings #-}


module System.Hardware.Leap.Event.Hand (
-- * Events
  Hand(..)
, Side(..)
) where


import Control.Applicative (empty)
import Data.Aeson (FromJSON(..), Value(..), (.:))
import System.Hardware.Leap.Types (Basis, LeapId, Matrix, Vector)


-- | Hands.
data Side = LeftHand | RightHand
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance FromJSON Side where
  parseJSON (String "left" ) = return LeftHand
  parseJSON (String "right") = return RightHand
  parseJSON _                = empty


-- | Hand tracking information.  See \<<https://developer.leapmotion.com/documentation/javascript/supplements/Leap_JSON.html>\> for details.
data Hand a =
    HandReference
    {
      leapId                 :: LeapId
    }
  | Hand
    {
      armBasis               :: Basis a
    , armWidth               :: a
    , confidence             :: a
    , direction              :: Vector a
    , elbow                  :: Vector a
    , grabStrength           :: a
    , leapId                 :: LeapId
    , palmNormal             :: Vector a
    , palmPosition           :: Vector a
    , palmVelocity           :: Vector a
    , pinchStrength          :: a
    , r                      :: Matrix a
    , s                      :: a
    , sphereCenter           :: Vector a
    , sphereRadius           :: a
    , stabilizedPalmPosition :: Vector a
    , t                      :: Vector a
    , timeVisible            :: a
    , side                   :: Side
    , wrist                  :: Vector a
    }
    deriving (Eq, Ord, Read, Show)

instance FromJSON a => FromJSON (Hand a) where 
  parseJSON (Object o) =
    Hand
      <$> o .: "armBasis"
      <*> o .: "armWidth"
      <*> o .: "confidence"
      <*> o .: "direction"
      <*> o .: "elbow"
      <*> o .: "grabStrength"
      <*> o .: "id"
      <*> o .: "palmNormal"
      <*> o .: "palmPosition"
      <*> o .: "palmVelocity"
      <*> o .: "pinchStrength"
      <*> o .: "r"
      <*> o .: "s"
      <*> o .: "sphereCenter"
      <*> o .: "sphereRadius"
      <*> o .: "stabilizedPalmPosition"
      <*> o .: "t"
      <*> o .: "timeVisible"
      <*> o .: "type"
      <*> o .: "wrist"
  parseJSON _ = empty
