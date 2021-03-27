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
    wr' = guardA sel $ fmap pack <$> wr
    rd' = singlePort (blockRamFile (SNat @DiskSize) "_build/disk.tracks") addr wr'
    rd = unpack <$> rd'

    addr = base + (fromIntegral <$> offset)

    tick = riseRate (SNat @FastRate)
    offset = regEn (0 :: Index TrackSize) tick $ nextIdx <$> offset

    track = snatToNum (SNat @TrackSize)

    base = regEn (0 :: Index DiskSize) sel $ do
        phase0 <- regEn 0 sel phase
        phase <- phase
        base <- base
        pure $
            let stepOut = satSub SatBound base track
                stepIn = satAdd SatBound base track
            in case (phase0, phase) of
                (0b110, 0b011) -> stepOut
                (0b011, 0b101) -> stepOut
                (0b101, 0b110) -> stepOut

                (0b110, 0b101) -> stepIn
                (0b101, 0b011) -> stepIn
                (0b011, 0b110) -> stepIn

                _ -> base
