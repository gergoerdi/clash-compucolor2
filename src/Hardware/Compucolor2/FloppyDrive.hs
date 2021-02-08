{-# LANGUAGE ApplicativeDo #-}
module Hardware.Compucolor2.FloppyDrive where

import Clash.Prelude
import RetroClash.Utils
import Text.Printf

import Control.Monad.State

floppyDrive
    :: forall dom. (HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (BitVector 3)
    -> Signal dom (Maybe Bit)
    -> Signal dom Bit
floppyDrive sel phase wr = rd
  where
    tick = riseEvery (SNat @26) -- TODO

    track = regMaybe (0 :: Index 41) $ do
        phase0 <- regEn 0 sel phase
        sel <- sel
        phase <- phase
        track0 <- track
        pure $ do
            guard sel
            let stepOut = Just $ lessIdx track0
                stepIn = Just $ moreIdx track0
            case (phase0, phase) of
                (1, 4) -> stepOut
                (4, 2) -> stepOut
                (2, 1) -> stepOut

                (1, 2) -> stepIn
                (2, 4) -> stepIn
                (4, 1) -> stepIn

                _ -> Nothing

    addr = regMaybe (0 :: Index 30720) $ enable tick $ nextIdx <$> addr

    -- mems :: Vec 41 (Signal dom Bit)
    -- mems = map (\i -> unpack <$> blockRamFile (SNat @30720) (printf "_build/disk-%02d.bin" i) addr (pure Nothing)) indicesI

    -- rd = bundle mems .!!. track

    rd = unpack <$> blockRamFile (SNat @30720) ("_build/disk/01.track") addr (pure Nothing)
