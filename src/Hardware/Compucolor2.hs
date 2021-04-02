{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module Hardware.Compucolor2
    ( topEntity
    , simEntity
    , mainBoard
    , simBoard

    , VidAddr
    ) where

import Clash.Prelude
import Clash.Annotations.TH

import Hardware.Compucolor2.TMS5501 as TMS5501
import Hardware.Compucolor2.CRT5027 as CRT5027
import Hardware.Compucolor2.Keyboard
import Hardware.Compucolor2.Video
import Hardware.Compucolor2.FloppyDrive
import Hardware.Intel8080.CPU

import RetroClash.Utils
import RetroClash.VGA
import RetroClash.PS2
import RetroClash.Memory
import RetroClash.Barbies
import Data.Maybe (isJust)

topEntity
    :: "CLK_40MHZ" ::: Clock Dom40
    -> "RESET"     ::: Reset Dom40
    -> "SWITCHES"  ::: Signal Dom40 (BitVector 8)
    -> "RX"        ::: Signal Dom40 Bit
    -> "PS2"       ::: PS2 Dom40
    -> ( "VGA"     ::: VGAOut Dom40 8 8 8
       , "TX"      ::: Signal Dom40 Bit
       )
topEntity = withEnableGen board
  where
    board switches rx ps2 = (vga, tx)
      where
        turbo = bitToBool . lsb <$> switches
        scanCode = parseScanCode . decodePS2 . samplePS2 $ ps2
        kbdCols = keyboard scanCode kbdRow

        (tx, kbdRow, crtOut, vidAddr, vidWrite) = mainBoard turbo rx kbdCols frameEnd vidRead
        (vga, frameEnd, vidRead) = video crtOut vidAddr vidWrite

mainBoard
    :: (HiddenClockResetEnable dom, KnownNat (DomainPeriod dom), 1 <= DomainPeriod dom)
    => Signal dom Bool
    -> Signal dom Bit
    -> Signal dom (BitVector 8)
    -> Signal dom Bool
    -> Signal dom (Maybe (Unsigned 8))
    -> ( Signal dom Bit
       , Signal dom (BitVector 8)
       , Signals dom CRT5027.Output
       , Signal dom (Maybe (Bool, VidAddr))
       , Signal dom (Maybe (Unsigned 8))
       )
mainBoard turbo rx kbdCols frameEnd vidRead = (tx, kbdRow, crtOut, vidAddr, vidWrite)
  where
    CPUOut{..} = intel8080 CPUIn{..}

    pause = not <$> (turbo .||. riseEvery (SNat @20))

    floppyRd = register 1 $ floppyDrive (fromActive @Low <$> sel) phase write
      where
        sel = bitCoerce . (`testBit` 4) <$> parallelOut
        wr = bitCoerce . (`testBit` 3) <$> parallelOut
        phase = slice (SNat @2) (SNat @0) <$> parallelOut
        write = enable (fromActive @Low <$> wr) serialOut

    tmsIn = TMS5501.MkInput
        { parallelIn = kbdCols
        , sensor = boolToBit . isJust <$> cursor
        , serialIn = floppyRd
        , ack = _interruptAck
        }
    kbdRow = parallelOut
    tx = serialOut

    (dataIn, ((vidAddr, vidWrite), crtOut@CRT5027.MkOutput{..}, TMS5501.MkOutput{..})) =
        $(memoryMap @(Either (Unsigned 8) (Unsigned 16)) [|_addrOut|] [|_dataOut|] $ do
            rom <- romFromFile (SNat @0x4000) [|"_build/v678.rom.bin"|]
            ram <- ram0 (SNat @0x4000)
            (vid, vidAddr, vidWrite) <- conduit @(Bool, VidAddr) [|vidRead|]

            -- TODO: how can we pattern match on tmsOut?
            (tms, tmsOut) <- port @TMS5501.Port [| tms5501 tmsIn |]
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

            return ((vidAddr, vidWrite), crtOut, tmsOut))

simBoard
    :: (HiddenClockResetEnable dom, KnownNat (DomainPeriod dom), 1 <= DomainPeriod dom)
    => "VID_READ"    ::: Signal dom (Maybe (Unsigned 8))
    -> ( "VID_ADDR"  ::: Signal dom (Maybe (Bool, VidAddr))
       , "VID_WRITE" ::: Signal dom (Maybe (Unsigned 8))
       )
simBoard vidRead = (vidAddr, vidWrite)
  where
    (_tx, _kbdRow, _crtOut, vidAddr, vidWrite) = mainBoard (pure True) (pure high) (pure 0x00) (pure False) vidRead

simEntity
    :: "CLK_40MHZ"    ::: Clock Dom40
    -> "VID_READ"     ::: Signal Dom40 (Maybe (Unsigned 8))
    -> ( "VID_ADDR"   ::: Signal Dom40 (Maybe (Bool, VidAddr))
       , "VID_WRITE"  ::: Signal Dom40 (Maybe (Unsigned 8))
       )
simEntity = withResetEnableGen simBoard

makeTopEntity 'topEntity
makeTopEntity 'simEntity
