{-# LANGUAGE OverloadedStrings #-}


module System.Hardware.Leap.Event.Hand (
  Hand(..)
, Side(..)
) where


import Control.Applicative (empty)
import Data.Aeson (FromJSON(..), Value(..), (.:))
import System.Hardware.Leap.Types (LeapId, Matrix, Vector)


data Side = LeftHand | RightHand
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance FromJSON Side where
  parseJSON (String "left" ) = return LeftHand
  parseJSON (String "right") = return RightHand
  parseJSON _                = empty


data Hand a =
    HandReference
    {
      leapId                 :: LeapId
    }
  | Hand
    {
      armBasis               :: Matrix a
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
