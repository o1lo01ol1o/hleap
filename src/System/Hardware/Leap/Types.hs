{-# LANGUAGE OverloadedStrings #-}


module System.Hardware.Leap.Types (
  Vector
, Matrix
, InteractionBox(..)
) where


import Control.Applicative (empty)
import Data.Aeson (FromJSON(..), Value(..), (.:))


type Vector a = (a, a, a)


type Matrix a = (Vector a, Vector a, Vector a)


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
  parseJSON _x = empty -- error $ "Failed to parse JSON: " ++ show _x
