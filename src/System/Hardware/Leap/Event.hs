{-# LANGUAGE OverloadedStrings #-}


module System.Hardware.Leap.Event (
  Event(..)
, State(..)
, InteractionBox(..)
) where


import Control.Applicative (empty)
import Data.Aeson (FromJSON(..), Value(..), (.:))
import System.Hardware.Leap.Event.Gesture (Gesture)
import System.Hardware.Leap.Event.Hand (Hand)
import System.Hardware.Leap.Event.Pointable (Pointable)
import System.Hardware.Leap.Types (Matrix, Vector)

import qualified Data.HashMap.Strict as M (lookup)


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
    , leapId           :: a
    , r                :: Matrix a
    , s                :: a
    , t                :: Vector a
    , timestamp        :: Int
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
      Nothing          -> Tracking
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
