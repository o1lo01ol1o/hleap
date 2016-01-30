{-# LANGUAGE RecordWildCards   #-}


module Main (
  main
) where


import Data.Default (def)
import System.Hardware.Leap (runWithHandler, setFocused, setGestures)
import System.Hardware.Leap.Event (Event(..))
import System.Hardware.Leap.Event.Hand (Hand(side))
import System.Hardware.Leap.Event.Gesture (Gesture(..))
import System.Hardware.Leap.Event.Pointable (Pointable(..))


main :: IO ()
main = runWithHandler def [setFocused True, setGestures True] handler


handler :: Event Float -> IO ()
handler Tracking{..}
  | not (null gestures  ) = mapM_ handleGesture gestures
  | not (null pointables) = mapM_ handlePointable pointables
  | otherwise             = return ()
handler _ = return ()


handlePointable :: Show a => Pointable a -> IO ()
handlePointable Finger{..}           = putStrLn $ "FINGER\t " ++ show (side hand) ++ "\t" ++ show finger ++ "\t" ++ show stabilizedTipPosition
handlePointable Tool{..}             = putStrLn $ "TOOL\t " ++ show (side hand) ++ "\t" ++ show stabilizedTipPosition
handlePointable PointableReference{} = return ()


handleGesture :: Gesture a -> IO ()
handleGesture Circle{..}         = putStrLn $ "CIRCLE\t "    ++ show state ++ "\t" ++ show (map side hands)
handleGesture Swipe{..}          = putStrLn $ "SWIPE\t"      ++ show state ++ "\t" ++ show (map side hands)
handleGesture KeyTap{..}         = putStrLn $ "KEY TAP\t"    ++ show state ++ "\t" ++ show (map side hands)
handleGesture ScreenTap{..}      = putStrLn $ "SCREEN TAP\t" ++ show state ++ "\t" ++ show (map side hands)
handleGesture GestureReference{} = return ()
