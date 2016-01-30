module System.Hardware.Leap.Types (
  Vector
, Matrix
) where


type Vector a = (a, a, a)


type Matrix a = (Vector a, Vector a, Vector a)
