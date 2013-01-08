-----------------------------------------------------------------------------
--
-- Module      :  Bitmap
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
--
-- |
-----------------------------------------------------------------------------

module Bitmap (
    RawColor,
    writeBitmap,
--    display
) where

import qualified Data.ByteString (pack)
import Vector
import Codec.BMP
import Control.Monad (forM_)
import Data.Word

type RawColor = Vector

toRGBA :: RawColor -> [Word8]
toRGBA (Vector (a1, a2, a3)) = [normalizeColor a1, normalizeColor a2, normalizeColor a3, 0]

normalizeColor :: Double -> Word8
normalizeColor x =
    max 0 (round ((x/(1+x)) * 255))

writeBitmap :: [RawColor] -> (Int, Int) -> String -> IO ()
writeBitmap bitmap (resX, resY) filename = writeBMP filename bmp
    where
        bytestring = Data.ByteString.pack . concat $ map toRGBA bitmap
        bmp        = packRGBA32ToBMP resX resY bytestring

