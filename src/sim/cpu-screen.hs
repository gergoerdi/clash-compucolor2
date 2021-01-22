{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}
{-# LANGUAGE PartialTypeSignatures #-}
import Clash.Prelude hiding ((!), lift)

import RetroClash.Sim.SDL
import RetroClash.Sim.IO
import Hardware.Compucolor2.Video
import Hardware.Compucolor2

import Data.Array.IO
import Data.Array ((!))
import qualified Data.ByteString as BS
import Control.Monad
import Control.Monad.Trans
import Data.Char (ord)
import qualified Data.List as L
import Data.Foldable (traverse_)
import Data.Traversable (for)

import Data.Maybe (isJust)
import Text.Printf

divI
    :: (KnownNat n, KnownNat k, 1 <= k, n ~ ((n `Div` k) * k))
    => SNat k
    -> Index n
    -> (Index (n `Div` k), Index k)
divI k@SNat x = let (x1, x0) = x `quotRem` snatToNum k
                in (fromIntegral x1, fromIntegral x0)

main :: IO ()
main = do
    fontPath <- return "image/chargen.uf7"
    fontBS <- BS.readFile fontPath
    fontROM <- newArray @IOArray @(Unsigned 8) @_ @(Index 1024) (minBound, maxBound) 0
    zipWithM_ (writeArray fontROM) [0..] (fmap bitCoerce $ BS.unpack fontBS)

    vidRAM <- newArray @IOArray @(Unsigned 8) @_ @(Index (TextWidth * TextHeight * 2)) (minBound, maxBound) 0x20

    sim <- simulateIO_ @System (bundle . mainBoard) Nothing

    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        replicateM_ 20000 $ liftIO $ do
            sim $ \(vidAddr, vidWrite) -> for vidAddr $ \(prio, addr) -> do
                x <- readArray vidRAM addr
                traverse_ (writeArray vidRAM addr) vidWrite
                return x

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
