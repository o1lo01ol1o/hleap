{-# LANGUAGE OverloadedStrings #-}


module System.Hardware.Leap.Event.Gesture (
  Gesture(..)
, State(..)
) where


import Control.Applicative (empty)
import Data.Aeson (FromJSON(..), Value(..), (.:))
import System.Hardware.Leap.Types (Vector)

import qualified Data.HashMap.Strict as M (lookup)


data State = Start | Update | Stop
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance FromJSON State where
  parseJSON (String "start" ) = return Start
  parseJSON (String "update") = return Update
  parseJSON (String "stop"  ) = return Stop
  parseJSON _                 = empty


data Gesture a =
    Circle
    {
      center       :: Vector a
    , duration     :: Int
    , handIds      :: [Int]
    , leapId       :: Int
    , normal       :: Vector a
    , pointableIds :: [Int]
    , progress     :: a
    , radius       :: a
    , state        :: State
    }
  | Swipe
    {
      direction     :: Vector a
    , duration      :: Int
    , handIds       :: [Int]
    , leapId        :: Int
    , pointableIds  :: [Int]
    , position      :: Vector a
    , speed         :: a
    , startPosition :: Vector a
    , state         :: State
    }
  | KeyTap
    {
      direction     :: Vector a
    , duration      :: Int
    , handIds       :: [Int]
    , leapId        :: Int
    , pointableIds  :: [Int]
    , position      :: Vector a
    , progress      :: a
    , state         :: State
    }
  | ScreenTap
    {
      direction     :: Vector a
    , duration      :: Int
    , handIds       :: [Int]
    , leapId        :: Int
    , pointableIds  :: [Int]
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
                                <*> o .: "handIds"
                                <*> o .: "id"
                                <*> o .: "normal"
                                <*> o .: "pointableIds"
                                <*> o .: "progress"
                                <*> o .: "radius"
                                <*> o .: "state"
    | t == Just "swipe"     = Swipe
                                <$> o .: "direction"
                                <*> o .: "duration"
                                <*> o .: "handIds"
                                <*> o .: "id"
                                <*> o .: "pointableIds"
                                <*> o .: "position"
                                <*> o .: "speed"
                                <*> o .: "startPosition"
                                <*> o .: "state"
    | t == Just "keyTap"    = KeyTap
                                <$> o .: "direction"
                                <*> o .: "duration"
                                <*> o .: "handIds"
                                <*> o .: "id"
                                <*> o .: "pointableIds"
                                <*> o .: "position"
                                <*> o .: "progress"
                                <*> o .: "state"
    | t == Just "screenTap" = ScreenTap
                                <$> o .: "direction"
                                <*> o .: "duration"
                                <*> o .: "handIds"
                                <*> o .: "id"
                                <*> o .: "pointableIds"
                                <*> o .: "position"
                                <*> o .: "progress"
                                <*> o .: "state"
    | otherwise             =  error $ "Failed to parse JSON: " ++ show (Object o)
      where
        t = "type" `M.lookup` o
  parseJSON _ = empty
