module System.Hardware.Leap.Types (
  LeapId
, Duration
, Vector
, Matrix
) where


type LeapId = Int


type Duration = Int


type Vector a = (a, a, a)


type Matrix a = (Vector a, Vector a, Vector a)
