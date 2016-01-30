{-# LANGUAGE OverloadedStrings #-}


module System.Hardware.Leap.Event.Pointable (
  Pointable(..)
, TouchZone(..)
, Finger(..)
) where


import Control.Applicative (empty)
import Data.Aeson (FromJSON(..), Value(..), (.:))
import System.Hardware.Leap.Types (Vector)

import qualified Data.HashMap.Strict as M (lookup)


data TouchZone = None | Hovering | Touching
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance FromJSON TouchZone where
  parseJSON (String "none"    ) = return None
  parseJSON (String "hovering") = return Hovering
  parseJSON (String "touching") = return Touching
  parseJSON _                   = empty


data Finger = Thumb | IndexFinger | MiddleFinger | RingFinger | Pinky
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance FromJSON Finger where
  parseJSON (Number 0) = return Thumb
  parseJSON (Number 1) = return IndexFinger
  parseJSON (Number 2) = return MiddleFinger
  parseJSON (Number 3) = return RingFinger
  parseJSON (Number 4) = return Pinky
  parseJSON _          = empty


data Pointable a =
    Finger
    {
      bases                 :: [[Vector a]]
    , btipPosition          :: Vector a
    , carpPosition          :: Vector a
    , dipPosition           :: Vector a
    , direction             :: Vector a
    , extended              :: Bool
    , handId                :: Int
    , leapId                :: Int
    , pointableLength       :: a
    , mcpPosition           :: Vector a
    , pipPosition           :: Vector a
    , stabilizedTipPosition :: Vector a
    , timeVisible           :: a
    , tipPosition           :: Vector a
    , tipVelocity           :: Vector a
    , touchDistance         :: a
    , touchZone             :: TouchZone
    , finger                :: Finger
    , width                 :: a
    }
  | Tool
    {
      direction             :: Vector a
    , handId                :: Int
    , leapId                :: Int
    , pointableLength       :: a
    , stabilizedTipPosition :: Vector a
    , timeVisible           :: a
    , tipPosition           :: Vector a
    , tipVelocity           :: Vector a
    , touchDistance         :: a
    , touchZone             :: TouchZone
    , width                 :: a
    }
    deriving (Eq, Ord, Read, Show)

instance FromJSON a => FromJSON (Pointable a) where
  parseJSON (Object o)
    | "tool" `M.lookup` o == Just (Bool True) = Tool
                                                  <$> o .: "direction"
                                                  <*> o .: "handId"
                                                  <*> o .: "id"
                                                  <*> o .: "length"
                                                  <*> o .: "stabilizedTipPosition"
                                                  <*> o .: "timeVisible"
                                                  <*> o .: "tipPosition"
                                                  <*> o .: "tipVelocity"
                                                  <*> o .: "touchDistance"
                                                  <*> o .: "touchZone"
                                                  <*> o .: "width"
    | otherwise                               = Finger
                                                  <$> o .: "bases"
                                                  <*> o .: "btipPosition"
                                                  <*> o .: "carpPosition"
                                                  <*> o .: "dipPosition"
                                                  <*> o .: "direction"
                                                  <*> o .: "extended"
                                                  <*> o .: "handId"
                                                  <*> o .: "id"
                                                  <*> o .: "length"
                                                  <*> o .: "mcpPosition"
                                                  <*> o .: "pipPosition"
                                                  <*> o .: "stabilizedTipPosition"
                                                  <*> o .: "timeVisible"
                                                  <*> o .: "tipPosition"
                                                  <*> o .: "tipVelocity"
                                                  <*> o .: "touchDistance"
                                                  <*> o .: "touchZone"
                                                  <*> o .: "type"
                                                  <*> o .: "width"
  parseJSON _ = empty
