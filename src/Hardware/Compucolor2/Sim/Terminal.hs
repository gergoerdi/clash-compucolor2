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
    for_ [minBound..maxBound] $ \y -> do
        setCursorPosition $ Position (fromIntegral y) 0
        for_ [minBound..maxBound] $ \x ->
            putCharAt x y
    flush
  where
    putCharAt :: Index TextWidth -> Index TextHeight -> m ()
    putCharAt x y = do
        let addr = bitCoerce (y, x, (0 :: Index 2))
        b0 <- liftIO $ readArray vidRAM addr
        b1 <- liftIO $ readArray vidRAM (addr + 1)
        let (tall, c) = bitCoerce b0
            (isChar, blink, back, fore) = bitCoerce @_ @(Bool, Bool, _, _) b1
        setAttribute $ foreground $ bright $ toColor fore
        setAttribute $ background $ bright $ toColor back
        putCharCC $ if tall && odd y then 0x20 else c

    putCharCC :: Unsigned 7 -> m ()
    putCharCC = putChar . chr . fromIntegral

    toColor :: Unsigned 3 -> Color m
    toColor 0b000 = black
    toColor 0b001 = red
    toColor 0b010 = green
    toColor 0b011 = yellow
    toColor 0b100 = blue
    toColor 0b101 = magenta
    toColor 0b110 = cyan
    toColor 0b111 = white
