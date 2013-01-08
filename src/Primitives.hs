-----------------------------------------------------------------------------
--
-- Module      :  Primitives
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

module Primitives where

import Vector
import Data.List (sort)
import Data.Maybe (fromJust)
import Bitmap

class Renderable a where
    getIntersection :: a -> Ray -> Maybe Double
    getNormal       :: a -> Vector -> Vector

data Ray = Ray
    { rOrigin    :: Vector
    , rDirection :: Vector
    } deriving Show

data Light = Light
    { lPosition  :: Vector
    , lIntensity :: Double
    } deriving Show

data Material = Material
    { mColor            :: RawColor
    , mAmbientStrength
    , mDiffuseStrength
    , mSpecularStrength
    , mSpecularExp
    , mReflectStrength  :: Double
    } deriving Show

data Sphere = Sphere
    { sRadius   :: Double
    , sCenter   :: Vector
    , sMaterial :: Material
    } deriving Show

cullDist :: [Double] -> Maybe Double
cullDist d =
    if null posDist
        then Nothing
        else Just (head posDist)
    where posDist = filter (>=0) $ sort d

--if the ray is normalized, a=1
instance Renderable Sphere where
    getIntersection sphere ray = let
        relativeOrigin = rOrigin ray - sCenter sphere
        --a = dot (rDirection ray) (rDirection ray)
        b = 2 * dot (rDirection ray) relativeOrigin
        c = dot relativeOrigin relativeOrigin - sRadius sphere ^ 2
        d = b^2 - 4*c
        t0 = if b < 0 then ((-b) + sqrt d)/2 else ((-b) - sqrt d)/2
        t1 = c / t0
        in if d < 0 then Nothing else cullDist [t0, t1]
    getNormal sphere point = normalize (point - sCenter sphere)

