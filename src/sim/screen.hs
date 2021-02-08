{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
import Clash.Prelude hiding ((!), lift)

import RetroClash.Sim.SDL
import Hardware.Compucolor2.Sim

import Data.Array.IO
import Data.Array ((!))
import qualified Data.ByteString as BS
import Control.Monad
import Control.Monad.Trans
import Data.Char (ord)
import qualified Data.List as L

type TextWidth = 64
type TextHeight = 32
type FontWidth = 6
type FontHeight = 8

main :: IO ()
main = do
    fontPath <- return "image/chargen.uf7"
    fontBS <- BS.readFile fontPath
    fontROM <- newArray @IOArray @(Unsigned 8) @_ @(Index 1024) (minBound, maxBound) 0
    zipWithM_ (writeArray fontROM) [0..] (fmap bitCoerce $ BS.unpack fontBS)

    vidRAM <- newArray @IOArray @(Unsigned 8) @_ @(Index (TextWidth * TextHeight * 2)) (minBound, maxBound) 0x20

    -- First two lines: full alphabet
    forM_ [0..127] $ \i -> do
        writeArray vidRAM (fromIntegral i * 2) i

    -- Next four lines: full alphabet, tall
    forM_ [0..63] $ \i -> do
        writeArray vidRAM (256 + fromIntegral i * 2)       (i + 128)
        writeArray vidRAM (256 + fromIntegral i * 2 + 128) (i + 128)
    forM_ [0..63] $ \i -> do
        writeArray vidRAM (512 + fromIntegral i * 2)       (i + 64 + 128)
        writeArray vidRAM (512 + fromIntegral i * 2 + 128) (i + 64 + 128)

    forM_ (L.zip [0..] "Compucolor 2") $ \(i, c) -> do
        let v = fromIntegral (ord c) + 128
        writeArray vidRAM (512 + 256 + fromIntegral i * 2)       v
        writeArray vidRAM (512 + 256 + fromIntegral i * 2 + 128) v

    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        fontROM <- lift $ freeze fontROM
        vidRAM <- lift $ freeze vidRAM
        return $ rasterizePattern @(TextWidth * FontWidth) @(TextHeight * FontHeight) $ \x y ->
          let (x1, x0) = divI (SNat @FontWidth) x
              (y1, y0) = divI (SNat @FontHeight) y
              charAddr = bitCoerce (y1, x1, (0 :: Index 2)) :: Index (TextWidth * TextHeight * 2)
              (tall, c) = bitCoerce (vidRAM ! charAddr) :: (Bool, Unsigned 7)
              y0' = if tall then (y0 `shiftR` 1 + if odd y1 then 4 else 0) else y0
              glyphAddr = bitCoerce (c, y0')
              glyphRow = fontROM ! glyphAddr
              pixel = testBit glyphRow (7 - fromIntegral x0)
              g = if even x1 `xor` even y1 then 0x60 else minBound
          in if pixel then (maxBound, maxBound, maxBound) else (minBound, g, minBound)

videoParams :: VideoParams
videoParams = MkVideoParams
    { windowTitle = "Compucolor 2"
    , screenScale = 4
    , screenRefreshRate = 60
    , reportFPS = True
    }
