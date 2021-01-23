{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module Hardware.Compucolor2
    ( topEntity
    , mainBoard
    , simBoard
    ) where

import Clash.Prelude
import Clash.Annotations.TH

import Hardware.Compucolor2.CRT5027
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
        (crtOut, vidAddr, vidWrite) = mainBoard vidRead
        (vga, frameEnd, vidRead) = video crtOut vidAddr vidWrite

mainBoard
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8))
    -> ( CRTOut dom
       , Signal dom (Maybe (Bool, VidAddr))
       , Signal dom (Maybe (Unsigned 8))
       )
mainBoard vidRead = (crtOut, vidAddr, vidWrite)
  where
    CPUOut{..} = intel8080 CPUIn{..}

    interruptRequest = pure False
    rst = pure Nothing

    (dataIn, (crtOut, vidAddr, vidWrite)) = $(memoryMap @(Either (Unsigned 8) (Unsigned 16)) [|_addrOut|] [|_dataOut|] $ do
        rom <- romFromFile (SNat @0x4000) [|"_build/v678.rom.bin"|]
        ram <- ram0 (SNat @0x8000)
        (vid, vidAddr, vidWrite) <- conduit @(Bool, VidAddr) [|vidRead|]

        tms <- readWrite_ @(Index 0x10) (\_ _ -> [|pure $ Just 0x00|]) -- TODO
        (crt, crtOut) <- port @(Index 0x10) [| crt5027 (pure False) |]
        prom <- readWrite_ @(Index 0x20) (\_ _ -> [|pure $ Just 0x00|]) -- TODO

        override [|rst|]

        matchLeft $ do
            from 0x00 $ connect tms
            from 0x10 $ connect tms
            from 0x60 $ connect crt
            from 0x70 $ connect crt
            from 0x80 $ connect prom

        matchRight $ do
            from 0x0000 $ connect rom
            from 0x6000 $ tag True $ connect vid
            from 0x7000 $ tag False $ connect vid
            from 0x8000 $ connect ram

        return (crtOut, vidAddr, vidWrite))

simBoard
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8))
    -> ( Signal dom (Maybe (Bool, VidAddr))
       , Signal dom (Maybe (Unsigned 8))
       )
simBoard vidRead = (vidAddr, vidWrite)
  where
    (_crtOut, vidAddr, vidWrite) = mainBoard vidRead

makeTopEntity 'topEntity
