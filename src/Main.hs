-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main where
import Bitmap
import Camera
import Primitives
import Scene
import Vector

testMaterial = Material (Vector (0, 0, 0)) 0.0 1.0 0.0 50 0.8
testSphere1 = Sphere 1 (Vector (-6, 0, 6)) testMaterial {mColor = Vector (1, 0, 0), mReflectStrength = 0.4}
testSphere2 = Sphere 5 (Vector (0, 0, 10)) testMaterial {mColor = Vector (1, 1, 1), mReflectStrength = 0.3, mDiffuseStrength = 0.4, mSpecularStrength = 0.3}

testLight = Light (Vector (0, 10, 5)) 2.0
testLight2 = Light (Vector (-3, 0, 0)) 2.0

testCam = Camera (Vector (0, 0, -10)) (Vector (0, 0, 10)) (Vector (16, 0, 0)) (Vector (0, 12, 0)) (1280, 960)
testScene = Scene [testSphere1, testSphere2] [testLight, testLight2]



main = writeBitmap (render testScene testCam) (1280, 960) "test.bmp"



