{-# LANGUAGE OverloadedStrings #-}
module Hardware.Compucolor2.Sim.Terminal where

import Clash.Prelude hiding ((!))

import System.Terminal

import Hardware.Compucolor2.Video
import Hardware.Compucolor2.CRT5027

import Data.Array.IO
import Control.Monad
import Data.Foldable (for_)
import Control.Monad.Trans
import Data.Char (chr)

putScreen
    :: forall m. (MonadIO m, MonadScreen m, MonadColorPrinter m)
    => IOArray VidAddr (Unsigned 8)
    -> m ()
putScreen vidRAM = do
    for_ [minBound..maxBound] $ \(y :: Index TextHeight) -> do
        setCursorPosition $ Position (fromIntegral y) 0

        for_ [minBound..maxBound] $ \(x :: Index TextWidth) -> do
            let addr = bitCoerce (y, x, (0 :: Index 2))
            b0 <- liftIO $ readArray vidRAM addr
            b1 <- liftIO $ readArray vidRAM (addr + 1)
            let (tall, c) = bitCoerce b0
                (isChar, blink, back, fore) = bitCoerce @_ @(Bool, Bool, _, _) b1
            setAttribute $ foreground $ bright $ toColor fore
            setAttribute $ background $ bright $ toColor back
            putCharCC $ if tall && odd y then 0x20 else c
    flush
  where
    putCharCC :: Unsigned 7 -> m ()
    putCharCC = putChar . chr . fromIntegral

    toColor :: (Bit, Bit, Bit) -> Color m
    toColor (0, 0, 0) = black
    toColor (0, 0, 1) = red
    toColor (0, 1, 0) = green
    toColor (0, 1, 1) = yellow
    toColor (1, 0, 0) = blue
    toColor (1, 0, 1) = magenta
    toColor (1, 1, 0) = cyan
    toColor (1, 1, 1) = white
