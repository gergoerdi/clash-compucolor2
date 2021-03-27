{-# LANGUAGE ApplicativeDo #-}
module Hardware.Compucolor2.FloppyDrive where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Clock
import Hardware.Compucolor2.TMS5501 (FastRate)

import Control.Monad.State

type TrackCount = 41
type TrackSize = 15360
type DiskSize = TrackCount * TrackSize

floppyDrive
    :: forall dom. (HiddenClockResetEnable dom, KnownNat (DomainPeriod dom), 1 <= DomainPeriod dom)
    => Signal dom Bool
    -> Signal dom (BitVector 3)
    -> Signal dom (Maybe Bit)
    -> Signal dom Bit
floppyDrive sel phase wr = mux sel rd (pure 1)
  where
    tick = riseEvery (SNat @(26 * 20)) -- TODO: compute this instead

    track = snatToNum (SNat @TrackSize)

    base = regMaybe (0 :: Index DiskSize) $ do
        phase0 <- regEn 0 sel phase
        sel <- sel
        phase <- phase
        base0 <- base
        pure $ do
            guard sel
            let stepOut = Just$ satSub SatBound base0 track
                stepIn = Just $ satAdd SatBound base0 track
            case (phase0, phase) of
                (0b110, 0b011) -> stepOut
                (0b011, 0b101) -> stepOut
                (0b101, 0b110) -> stepOut

                (0b110, 0b101) -> stepIn
                (0b101, 0b011) -> stepIn
                (0b011, 0b110) -> stepIn

                _ -> Nothing

    offset = regMaybe (0 :: Index TrackSize) $ enable tick $ nextIdx <$> offset
    addr = base + (fromIntegral <$> offset)

    rd = unpack <$> blockRamFile (SNat @DiskSize) ("_build/disk.tracks") addr (pure Nothing)
