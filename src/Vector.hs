-----------------------------------------------------------------------------
--
-- Module      :  Vector
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Vector where

(~~) :: Double -> Double -> Bool
a ~~ b = abs (a - b) < epsilon

epsilon = 0.000001

data Vector = Vector (Double, Double, Double) deriving Show

instance Eq (Vector) where
    (==) (Vector (a1, a2, a3)) (Vector (b1, b2, b3)) =
        (a1 ~~ b1) && (a2 ~~ b2) && (a3 ~~ b3)

instance Num (Vector) where
    (+) (Vector (a1, a2, a3)) (Vector (b1, b2, b3)) =
        Vector (a1+b1, a2+b2, a3+b3)
    (-) (Vector (a1, a2, a3)) (Vector (b1, b2, b3)) =
        Vector (a1-b1, a2-b2, a3-b3)
    (*) (Vector (a1, a2, a3)) (Vector (b1, b2, b3)) =
        Vector (a1*b1, a2*b2, a3*b3)
    abs (Vector (a1, a2, a3)) = Vector (abs a1, abs a2, abs a3)
    negate (Vector (a1, a2, a3)) = Vector (negate a1, negate a2, negate a3)
    fromInteger x = error "nope"
    signum (Vector (a1, a2, a3)) = Vector (signum a1, signum a2, signum a3)

(*|) :: Vector -> Double -> Vector
(*|) = flip (|*)

(|*) :: Double -> Vector -> Vector
(|*) x (Vector (a1, a2, a3)) = Vector (a1*x, a2*x, a3*x)

dot :: Vector -> Vector -> Double
dot (Vector (a1, a2, a3)) (Vector (b1, b2, b3)) = a1*b1 + a2*b2 + a3*b3

magnitude :: Vector -> Double
magnitude (Vector (a1, a2, a3)) = sqrt (a1*a1 + a2*a2 + a3*a3)

normalize :: Vector -> Vector
normalize v = v *| (1.0 / magnitude v)

epsAdjust :: Vector -> Vector -> Vector
epsAdjust origin direction = origin + (direction *| epsilon)
