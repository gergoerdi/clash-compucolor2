{-# LANGUAGE OverloadedStrings #-}
module Hardware.Compucolor2.Sim.SDL where

import Clash.Prelude hiding ((!))

import RetroClash.Utils
import RetroClash.Sim.SDL

import Hardware.Compucolor2.Sim
import Hardware.Compucolor2.Video
import Hardware.Compucolor2.CRT5027

import Data.Array.IO
import Data.Array (Array, (!))
import Data.Word

renderScreen
    :: Array (Index 1024) (Unsigned 8)
    -> IOArray VidAddr (Unsigned 8)
    -> IO (Rasterizer (TextWidth * FontWidth) (TextHeight * FontHeight))
renderScreen fontROM vidRAM = do
    vidRAM <- freeze vidRAM
    return $ rasterizePattern $ \x y ->
      let (x1, x0) = divI (SNat @FontWidth) x
          (y1, y0) = divI (SNat @FontHeight) y

          charAddr = bitCoerce (y1, x1, (0 :: Index 2))
          char = vidRAM ! charAddr
          attr = vidRAM ! (charAddr + 1)
          (tall, c) = bitCoerce char :: (Bool, Unsigned 7)
          (isPlot, (blink :: Bool), back, fore) = bitCoerce attr

          pixel
            | isPlot = testBit (char `shiftR` fromIntegral (half y0)) (if x0 < 3 then 0 else 4)
            | otherwise = testBit glyphRow (7 - fromIntegral x0)
            where
              y0' = if tall then half y0 + if odd y1 then 4 else 0 else y0
              glyphAddr = bitCoerce (c, y0')
              glyphRow = fontROM ! glyphAddr

          (r, g, b) = toColor $ if pixel then fore else back
          checker = if even x1 `xor` even y1 then 0x60 else minBound
      in (r, g `max` checker, b)

videoParams :: VideoParams
videoParams = MkVideoParams
    { windowTitle = "Compucolor II"
    , screenScale = 4
    , screenRefreshRate = 60
    , reportFPS = True
    }

toColor :: (Bit, Bit, Bit) -> (Word8, Word8, Word8)
toColor (b, g, r) = (stretch r, stretch g, stretch b)
  where
    stretch 0 = minBound
    stretch 1 = maxBound
