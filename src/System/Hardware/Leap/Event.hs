{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module System.Hardware.Leap.Event (
  Event(..)
, State(..)
, InteractionBox(..)
) where


import Control.Applicative (empty)
import Data.Aeson (FromJSON(..), Value(..), (.:))
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.Hardware.Leap.Event.Gesture (Gesture(..))
import System.Hardware.Leap.Event.Hand (Hand(HandReference))
import System.Hardware.Leap.Event.Pointable (Pointable(..))
import System.Hardware.Leap.Types (Duration, LeapId, Matrix, Vector)

import qualified Data.HashMap.Strict as M (lookup)
import qualified System.Hardware.Leap.Event.Gesture as G (hands, pointables)
import qualified System.Hardware.Leap.Event.Hand as H (leapId)
import qualified System.Hardware.Leap.Event.Pointable as P (hand, leapId)


data State =
  State
  {
    attached  :: Bool
  , leap      :: String
  , streaming :: Bool
  , leapType  :: String
  }
    deriving (Eq, Ord, Read, Show)

instance FromJSON State where
  parseJSON (Object o) =
    State
      <$> o .: "attached"
      <*> o .: "id"
      <*> o .: "streaming"
      <*> o .: "type"
  parseJSON _ = empty


data Event a =
    Event
    {
      state :: State
    , event :: String
    }
  | Tracking
    {
      currentFrameRate :: a
    , leapId           :: LeapId
    , r                :: Matrix a
    , s                :: a
    , t                :: Vector a
    , timestamp        :: Duration
    , devices          :: [String]
    , gestures         :: [Gesture a]
    , hands            :: [Hand a]
    , interactionBox   :: InteractionBox a
    , pointables       :: [Pointable a]
    }
      deriving (Eq, Ord, Read, Show)

instance FromJSON a => FromJSON (Event a) where
  parseJSON (Object o) =
    case "event" `M.lookup` o of
      Just (Object o') -> Event
                            <$> o' .: "state"
                            <*> o' .: "type"
      Nothing          -> tracking
                            <$> o .: "currentFrameRate"
                            <*> o .: "id"
                            <*> o .: "r"
                            <*> o .: "s"
                            <*> o .: "t"
                            <*> o .: "timestamp"
                            <*> o .: "devices"
                            <*> o .: "gestures"
                            <*> o .: "hands"
                            <*> o .: "interactionBox"
                            <*> o .: "pointables"
      _                -> empty
  parseJSON _ = empty


tracking :: a -> Int -> Matrix a -> a -> Vector a -> Int -> [String] -> [Gesture a] -> [Hand a] -> InteractionBox a -> [Pointable a] -> Event a
tracking currentFrameRate leapId r s t timestamp devices gestures' hands interactionBox pointables' =
  let
    replaceHand h@(HandReference i) = fromMaybe h $ find ((== i) . H.leapId) hands
    replaceHand h                   = h
    replacePointable p@(PointableReference i) = fromMaybe p $ find ((== i) . P.leapId) pointables
    replacePointable p                        = p
    updatePointable p@Finger{}             = p {P.hand = replaceHand $ P.hand p}
    updatePointable p@Tool{}               = p {P.hand = replaceHand $ P.hand p}
    updatePointable p@PointableReference{} = p
    updateGesture g@Circle{}           = g {G.hands = map replaceHand $ G.hands g, G.pointables = map replacePointable $ G.pointables g}
    updateGesture g@Swipe{}            = g {G.hands = map replaceHand $ G.hands g, G.pointables = map replacePointable $ G.pointables g}
    updateGesture g@KeyTap{}           = g {G.hands = map replaceHand $ G.hands g, G.pointables = map replacePointable $ G.pointables g}
    updateGesture g@ScreenTap{}        = g {G.hands = map replaceHand $ G.hands g, G.pointables = map replacePointable $ G.pointables g}
    updateGesture g@GestureReference{} = g
    pointables = map updatePointable pointables'
    gestures = map updateGesture gestures'
  in
    Tracking{..}


data InteractionBox a =
  InteractionBox
  {
    center :: Vector a
  , size   :: Vector a
  }
    deriving (Eq, Ord, Read, Show)

instance FromJSON a => FromJSON (InteractionBox a) where
  parseJSON (Object o) =
    InteractionBox
      <$> o .: "center"
      <*> o .: "size"
  parseJSON _ = empty
