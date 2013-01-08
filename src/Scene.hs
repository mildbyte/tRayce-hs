-----------------------------------------------------------------------------
--
-- Module      :  Scene
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

module Scene where

import Vector
import Primitives
import Bitmap
import Camera
import Data.List (foldl1', foldl')

data Scene = Scene
    { sRenderables :: [Sphere]
    , sLights      :: [Light]
    } deriving Show

backgroundColor = Vector (0,0,0)
ambientContrib = 0
reflectionDepth = 2
reflectionThreshold = 0.1

traceRay :: Scene -> Ray -> Int -> RawColor
traceRay scene ray@(Ray rayOr rayDir) currDepth =
    if (null intersections) || (currDepth > reflectionDepth)
        then backgroundColor
        else (mColor $ sMaterial closS) *| totalShading
             + ((mReflectStrength $ sMaterial closS) |* reflectedColor)
    where
        reflectedColor = traceReflectedRay scene interPoint rayDir (getNormal closS interPoint) currDepth
        totalShading   = ambientContrib + shadingCoef * (1-ambientContrib)
        shadingCoef    = getShadingCoefficient scene closS interPoint rayDir
        (closD, closS) = getClosestIntersection intersections
        interPoint     = rayOr + closD |* rayDir
        intersections  = getIntersections ray (sRenderables scene)

getShadingCoefficient :: Scene -> Sphere -> Vector -> Vector -> Double
getShadingCoefficient scene sphere interPoint viewDir =
    sum . map getOneLightContrib $ sLights scene
    where
        normal                                = getNormal sphere interPoint
        Material _ sAmb sDif sSpec sSpecExp _ = sMaterial sphere
        getOneLightContrib light = if lightIsVisible scene light interPoint
            then lIntensity light * (sAmb + sDif * diffuse + sSpec * specular)
            else 0
            where
                lightDir = normalize (lPosition light - interPoint)
                diffuse  = getDiffuseCoef lightDir normal
                specular = getSpecularCoef viewDir lightDir normal sSpecExp

getDiffuseCoef :: Vector -> Vector -> Double
getDiffuseCoef lightDir normal = max 0 (dot lightDir normal)

getSpecularCoef :: Vector -> Vector -> Vector -> Double -> Double
getSpecularCoef viewDir lightDir normal specExp =
    dot reflLight viewDir ** specExp
    where
        reflLight = reflectDir normal lightDir

reflectDir :: Vector -> Vector -> Vector
reflectDir normal origin = origin - ( (2 * dot origin normal) |* normal)

traceReflectedRay :: Scene -> Vector -> Vector -> Vector -> Int -> RawColor
traceReflectedRay scene interPoint viewDir normal currDepth =
    traceRay scene reflRay (currDepth + 1)
    where
        reflRay = Ray (epsAdjust interPoint reflDir) reflDir
        reflDir = reflectDir normal viewDir

lightIsVisible :: Scene -> Light -> Vector -> Bool
lightIsVisible scene light point =
    case () of () | null lightInters -> True
                  | fst (getClosestIntersection lightInters) > lightDist -> True
                  | otherwise -> False
    where
        lightVector = lPosition light - point
        lightDir    = normalize lightVector
        lightDist   = magnitude lightVector
        lightRay    = Ray (epsAdjust point lightDir) lightDir
        lightInters = getIntersections lightRay (sRenderables scene)

getIntersections :: Ray -> [Sphere] -> [(Double, Sphere)]
getIntersections ray =
    foldl' getOneInter []
    where
        getOneInter currList sphere =
            case getIntersection sphere ray of
            Nothing   -> currList
            Just dist -> (dist, sphere) : currList

getClosestIntersection :: [(Double, Sphere)] -> (Double, Sphere)
getClosestIntersection = foldl1'
    (\(d1, s1) (d2, s2) -> if d1 < d2 then (d1, s1) else (d2, s2))


tracePixel :: Scene -> OptCamera -> (Int, Int) -> RawColor
tracePixel scene camera coord = let
    rayOrigin    = ocPosition camera
    rayDirection = normalize (planeToWorldCoords camera coord)
    in
        traceRay scene (Ray rayOrigin rayDirection) 0

render :: Scene -> Camera -> [RawColor]
render scene camera =
    [tracePixel scene (optimizeCamera camera) (x, y) | y <- [0..resY-1], x <- [0..resX-1]]
    where
        resX = fst $ cResolution camera
        resY = snd $ cResolution camera
