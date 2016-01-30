{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Main (
  main
) where


import Data.Default (def)
import System.Hardware.Leap (run)
import System.Hardware.Leap.Event (Event(..))
import System.Hardware.Leap.Event.Hand (Hand(..), Side)
import System.Hardware.Leap.Event.Gesture (Gesture(..))
import System.Hardware.Leap.Event.Pointable (Pointable(..), Finger(IndexFinger))


main :: IO ()
main = run def handler


handler :: Event Float -> IO ()
handler Tracking{..} =
  do
{-
    let
      handIds = map (\Hand{..} -> (leapId, side)) hands
    mapM_ (handlePointable handIds) pointables
-}
    mapM_ handleGesture gestures
handler _ = return ()


handlePointable :: [(Int, Side)] -> Pointable Float -> IO ()
handlePointable handIds Finger{..}
  | finger == IndexFinger = print (hand, finger, stabilizedTipPosition)
  | otherwise             = return ()
handlePointable handIds t@Tool{..} = print ("Tool", stabilizedTipPosition)


handleGesture :: Gesture Float -> IO ()
handleGesture Circle{..}    = putStrLn $ "CIRCLE\t " ++ show state ++ " " ++ show pointables
handleGesture Swipe{..}     = putStrLn $ "SWIPE\t"   ++ show state ++ " " ++ show pointables
handleGesture KeyTap{..}    = putStrLn $ "KEY TAP\t" ++ show state ++ " " ++ show pointables
handleGesture ScreenTap{..} = putStrLn $ "SCREEN TAP\t" ++ show state ++ " " ++ show pointables
