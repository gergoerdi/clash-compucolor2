{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module Hardware.Compucolor2
    ( topEntity
    , simEntity
    , mainBoard
    , simBoard
    ) where

import Clash.Prelude
import Clash.Annotations.TH

import Hardware.Compucolor2.TMS5501 as TMS5501
import Hardware.Compucolor2.CRT5027 as CRT5027
import Hardware.Compucolor2.Keyboard
import Hardware.Compucolor2.Video
import Hardware.Intel8080.CPU

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
        scanCode = parseScanCode . decodePS2 . samplePS2 $ ps2

        (crtOut, vidAddr, vidWrite, kbdRow) = mainBoard scanCode frameEnd vidRead
        (vga, frameEnd, vidRead) = video crtOut vidAddr vidWrite

mainBoard
    :: (HiddenClockResetEnable dom, KnownNat (DomainPeriod dom), 1 <= DomainPeriod dom)
    => Signal dom (Maybe ScanCode)
    -> Signal dom Bool
    -> Signal dom (Maybe (Unsigned 8))
    -> ( Signals dom CRT5027.Output
       , Signal dom (Maybe (Bool, VidAddr))
       , Signal dom (Maybe (Unsigned 8))
       , Signal dom (Unsigned 8)
       )
mainBoard scanCode frameEnd vidRead = (crtOut, vidAddr, vidWrite, kbdRow)
  where
    CPUOut{..} = intel8080 CPUIn{..}

    kbdCols = keyboard scanCode kbdRow

    (dataIn, (crtOut@CRT5027.MkOutput{..}, (vidAddr, vidWrite), (kbdRow, interruptRequest, rst))) =
        $(memoryMap @(Either (Unsigned 8) (Unsigned 16)) [|_addrOut|] [|_dataOut|] $ do
            rom <- romFromFile (SNat @0x4000) [|"_build/v678.rom.bin"|]
            ram <- ram0 (SNat @0x8000)
            (vid, vidAddr, vidWrite) <- conduit @(Bool, VidAddr) [|vidRead|]

            -- TODO: how can we pattern match on tmsOut?
            (tms, tmsOut) <- port @TMS5501.Port [| tms5501 blink kbdCols _interruptAck |]
            (crt, crtOut) <- port @(Index 0x10) [| crt5027 frameEnd |]
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

            return (crtOut, (vidAddr, vidWrite), tmsOut))

simBoard
    :: (HiddenClockResetEnable dom, KnownNat (DomainPeriod dom), 1 <= DomainPeriod dom)
    => "VID_READ"    ::: Signal dom (Maybe (Unsigned 8))
    -> ( "VID_ADDR"  ::: Signal dom (Maybe (Bool, VidAddr))
       , "VID_WRITE" ::: Signal dom (Maybe (Unsigned 8))
       )
simBoard vidRead = (vidAddr, vidWrite)
  where
    (_crtOut, vidAddr, vidWrite, kbdRow) = mainBoard (pure Nothing) (pure False) vidRead

simEntity
    :: "CLK_40MHZ"    ::: Clock Dom40
    -> "VID_READ"     ::: Signal Dom40 (Maybe (Unsigned 8))
    -> ( "VID_ADDR"   ::: Signal Dom40 (Maybe (Bool, VidAddr))
       , "VID_WRITE"  ::: Signal Dom40 (Maybe (Unsigned 8))
       )
simEntity = withResetEnableGen simBoard

makeTopEntity 'topEntity
makeTopEntity 'simEntity
