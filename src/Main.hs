{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main (
  main
) where


import Data.Default (def)
import System.Hardware.Leap (run)
import System.Hardware.Leap.Event (Event(..))
import System.Hardware.Leap.Hand (Hand(..), Side)
import System.Hardware.Leap.Pointable (Pointable(..), Finger(IndexFinger))


main :: IO ()
main = run def handler


handler :: Event Float -> IO ()
handler Tracking{..} =
  do
    let
      handIds = map (\Hand{..} -> (leapId, side)) hands
    mapM_ (handlePointable handIds) pointables
handler _ = return ()


handlePointable :: [(Int, Side)] -> Pointable Float -> IO ()
handlePointable handIds Finger{..}
   | finger == IndexFinger = print (handId `lookup` handIds, finger, stabilizedTipPosition)
  | otherwise             = return ()
handlePointable handIds t@Tool{..} = print ("Tool", stabilizedTipPosition)
