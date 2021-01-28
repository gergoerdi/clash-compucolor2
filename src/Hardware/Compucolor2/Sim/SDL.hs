{-# LANGUAGE OverloadedStrings #-}
module Hardware.Compucolor2.Sim.SDL where

import Clash.Prelude hiding ((!))

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
    return $ rasterizePattern @(TextWidth * FontWidth) @(TextHeight * FontHeight) $ \x y ->
      let (x1, x0) = divI (SNat @FontWidth) x
          (y1, y0) = divI (SNat @FontHeight) y
          charAddr = bitCoerce (y1, x1, (0 :: Index 2)) :: Index (TextWidth * TextHeight * 2)
          (tall, c) = bitCoerce (vidRAM ! charAddr) :: (Bool, Unsigned 7)
          y0' = if tall then (y0 `shiftR` 1 + if odd y1 then 4 else 0) else y0
          glyphAddr = bitCoerce (c, y0')
          glyphRow = fontROM ! glyphAddr
          pixel = testBit glyphRow (7 - fromIntegral x0)
          attr = vidRAM ! (charAddr + 1)
          (isChar, blink, back, fore) = bitCoerce @_ @(Bool, Bool, _, _) attr
          (r, g, b) = toColor $ if pixel then fore else back
          checker = if even x1 `xor` even y1 then 0x60 else minBound
      in (r, g `max` checker, b)

videoParams :: VideoParams
videoParams = MkVideoParams
    { windowTitle = "Compucolor 2"
    , screenScale = 4
    , screenRefreshRate = 60
    , reportFPS = True
    }

toColor :: (Bit, Bit, Bit) -> (Word8, Word8, Word8)
toColor (b, g, r) = (stretch r, stretch g, stretch b)
  where
    stretch x = if x == 0 then minBound else maxBound
