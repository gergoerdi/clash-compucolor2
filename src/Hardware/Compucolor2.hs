{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module Hardware.Compucolor2
    ( topEntity
    ) where

import Clash.Prelude
import Clash.Annotations.TH

import Hardware.Compucolor2.Video
import Hardware.Intel8080.CPU
import Hardware.Intel8080.Interruptor

import RetroClash.Utils
import RetroClash.VGA
import RetroClash.PS2
import RetroClash.Memory
import RetroClash.Barbies
import Data.Maybe

topEntity
    :: "CLK_40MHZ" ::: Clock Dom40
    -> "RESET"     ::: Reset Dom40
    -> "PS2"       ::: PS2 Dom40
    -> "VGA"       ::: VGAOut Dom40 8 8 8
topEntity = withEnableGen board
  where
    board ps2 = vga
      where
        (vidAddr, vidWrite) = mainBoard vidRead
        (vga, vidRead) = video vidAddr vidWrite

mainBoard
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8))
    -> ( Signal dom (Maybe (Bool, VidAddr))
       , Signal dom (Maybe (Unsigned 8))
       )
mainBoard vidRead = (vidAddr, vidWrite)
  where
    vidAddr = pure Nothing
    vidWrite = pure Nothing

makeTopEntity 'topEntity
