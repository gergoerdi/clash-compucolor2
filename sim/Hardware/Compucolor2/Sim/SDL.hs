{-# LANGUAGE OverloadedStrings #-}
module Hardware.Compucolor2.Sim.SDL where

import Clash.Prelude hiding ((!))

import RetroClash.Utils
import RetroClash.Sim.SDL

import Hardware.Compucolor2.Sim
import Hardware.Compucolor2.Video
import Hardware.Compucolor2.Video.Plot
import Hardware.Compucolor2.CRT5027

import Data.Array.IO
import Data.Array (Array, (!))
import Data.Word

renderScreen
    :: Array (Index 1024) (BitVector 8)
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
          (isTall, c) = bitCoerce char :: (Bool, Unsigned 7)
          (isPlot, blink, back, fore) = attributes attr

          pixel = bitToBool $ msb $ block `shiftL` fromIntegral x0
          block = if isPlot then plotBlock else fontBlock

          plotBlock = stretchRow b1 b2
            where
              (char2, char1) = splitChar char
              b1 = plots !! plotAddr char1 (halfIndex y0)
              b2 = plots !! plotAddr char2 (halfIndex y0)

          fontBlock = fontROM ! glyphAddr
            where
              y0' = if isTall then toTall y1 y0 else y0
              glyphAddr = bitCoerce (c, y0')

          (r, g, b) = fromBGR $ if pixel then fore else back
          checker = if even x1 `xor` even y1 then 0x60 else minBound
      in (r, g `max` checker, b)

videoParams :: VideoParams
videoParams = MkVideoParams
    { windowTitle = "Compucolor II"
    , screenScale = 4
    , screenRefreshRate = 60
    , reportFPS = True
    }
