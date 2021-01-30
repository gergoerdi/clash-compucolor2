{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
import Clash.Prelude hiding ((!), lift)

import RetroClash.Sim.SDL
import Hardware.Compucolor2.Sim (divI)

import Data.Array.IO
import Data.Array ((!))
import qualified Data.ByteString as BS
import Control.Monad
import Control.Monad.Trans

type FontWidth = 6
type FontHeight = 8

main :: IO ()
main = do
    fontPath <- return "image/chargen.uf6"
    fontBS <- BS.readFile fontPath
    fontROM <- newArray @IOArray (minBound, maxBound) 0
    zipWithM_ (writeArray fontROM) [0..] (BS.unpack fontBS)

    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        fontROM <- lift $ freeze fontROM
        return $ rasterizePattern @(16 * FontWidth) @(8 * FontHeight) $ \x y ->
          let (x1, x0) = divI (SNat @FontWidth) x
              (y1, y0) = divI (SNat @FontHeight) y
              c = bitCoerce (y1, x1) :: Index 128
              addr = bitCoerce (c, y0) :: Index 1024
              row = fontROM ! addr
              pixel = testBit row (7 - fromIntegral x0)
              g = if even x1 `xor` even y1 then 0x60 else minBound
          in if pixel then (maxBound, maxBound, maxBound) else (minBound, g, minBound)

videoParams :: VideoParams
videoParams = MkVideoParams
    { windowTitle = "Compucolor 2"
    , screenScale = 10
    , screenRefreshRate = 60
    , reportFPS = True
    }
