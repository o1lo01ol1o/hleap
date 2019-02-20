{-|
Module      :  $Header$
Copyright   :  (c) 2016-19 Brian W Bush
License     :  MIT
Maintainer  :  Brian W Bush <code@functionally.io>
Stability   :  Stable
Portability :  Portable

Gesture events for Leap Motion \<<https://www.leapmotion.com/product/desktop>\>, based on the Web Socket API \<<https://developer.leapmotion.com/documentation/javascript/supplements/Leap_JSON.html>\>.
-}


{-# LANGUAGE OverloadedStrings #-}


module System.Hardware.Leap.Event.Gesture (
-- * Events
  Gesture(..)
, State(..)
) where


import Control.Applicative (empty)
import Data.Aeson (FromJSON(..), Value(..), (.:))
import System.Hardware.Leap.Event.Hand (Hand(HandReference))
import System.Hardware.Leap.Event.Pointable (Pointable(PointableReference))
import System.Hardware.Leap.Types (Duration, LeapId, Vector)

import qualified Data.HashMap.Strict as M (lookup)


-- | The state of a gesture.
data State = Start | Update | Stop
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance FromJSON State where
  parseJSON (String "start" ) = return Start
  parseJSON (String "update") = return Update
  parseJSON (String "stop"  ) = return Stop
  parseJSON _                 = empty


-- | Gesture tracking information.  See \<<https://developer.leapmotion.com/documentation/javascript/supplements/Leap_JSON.html>\> for details.
data Gesture a =
    GestureReference
    {
      leapId       :: LeapId
    }
  | Circle
    {
      center       :: Vector a
    , duration     :: Duration
    , hands        :: [Hand a]
    , leapId       :: LeapId
    , normal       :: Vector a
    , pointables   :: [Pointable a]
    , progress     :: a
    , radius       :: a
    , state        :: State
    }
  | Swipe
    {
      direction     :: Vector a
    , duration      :: Duration
    , hands         :: [Hand a]
    , leapId        :: LeapId
    , pointables    :: [Pointable a]
    , position      :: Vector a
    , speed         :: a
    , startPosition :: Vector a
    , state         :: State
    }
  | KeyTap
    {
      direction     :: Vector a
    , duration      :: Duration
    , hands         :: [Hand a]
    , leapId        :: LeapId
    , pointables    :: [Pointable a]
    , position      :: Vector a
    , progress      :: a
    , state         :: State
    }
  | ScreenTap
    {
      direction     :: Vector a
    , duration      :: Duration
    , hands         :: [Hand a]
    , leapId        :: LeapId
    , pointables    :: [Pointable a]
    , position      :: Vector a
    , progress      :: a
    , state         :: State
    }
    deriving (Eq, Ord, Read, Show)

instance FromJSON a => FromJSON (Gesture a) where
  parseJSON (Object o)
    | t == Just "circle"    = Circle
                                <$> o .: "center"
                                <*> o .: "duration"
                                <*> (map HandReference <$> o .: "handIds")
                                <*> o .: "id"
                                <*> o .: "normal"
                                <*> (map PointableReference <$> o .: "pointableIds")
                                <*> o .: "progress"
                                <*> o .: "radius"
                                <*> o .: "state"
    | t == Just "swipe"     = Swipe
                                <$> o .: "direction"
                                <*> o .: "duration"
                                <*> (map HandReference <$> o .: "handIds")
                                <*> o .: "id"
                                <*> (map PointableReference <$> o .: "pointableIds")
                                <*> o .: "position"
                                <*> o .: "speed"
                                <*> o .: "startPosition"
                                <*> o .: "state"
    | t == Just "keyTap"    = KeyTap
                                <$> o .: "direction"
                                <*> o .: "duration"
                                <*> (map HandReference <$> o .: "handIds")
                                <*> o .: "id"
                                <*> (map PointableReference <$> o .: "pointableIds")
                                <*> o .: "position"
                                <*> o .: "progress"
                                <*> o .: "state"
    | t == Just "screenTap" = ScreenTap
                                <$> o .: "direction"
                                <*> o .: "duration"
                                <*> (map HandReference <$> o .: "handIds")
                                <*> o .: "id"
                                <*> (map PointableReference <$> o .: "pointableIds")
                                <*> o .: "position"
                                <*> o .: "progress"
                                <*> o .: "state"
    | otherwise             =  empty
      where
        t = "type" `M.lookup` o
  parseJSON _ = empty
