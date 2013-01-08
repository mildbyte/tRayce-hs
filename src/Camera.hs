-----------------------------------------------------------------------------
--
-- Module      :  Camera
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

module Camera where

import Vector

data Camera = Camera
    { cPosition    :: Vector
    , cDirection   :: Vector
    , cImagePlaneX :: Vector
    , cImagePlaneY :: Vector
    , cResolution  :: (Int, Int)
    } deriving Show

optimizeCamera :: Camera -> OptCamera
optimizeCamera camera = let
    resX         = fromIntegral . fst $ cResolution camera
    resY         = fromIntegral . snd $ cResolution camera
    imagePlaneDX = cImagePlaneX camera *| (1.0/resX)
    imagePlaneDY = cImagePlaneY camera *| (1.0/resY)
    in OptCamera (cPosition camera) (cDirection camera) imagePlaneDX imagePlaneDY resX resY

data OptCamera = OptCamera
    { ocPosition      :: Vector
    , ocDirection    :: Vector
    , ocImagePlaneDX :: Vector
    , ocImagePlaneDY :: Vector
    , oResX, oResY    :: Double
    } deriving Show

planeToWorldCoords :: OptCamera -> (Int, Int) -> Vector
planeToWorldCoords (OptCamera pos dir dx dy resX resY) (x, y) = dir
    + dx *| (fromIntegral x - resX/2)
    + dy *| (fromIntegral y - resY/2)
