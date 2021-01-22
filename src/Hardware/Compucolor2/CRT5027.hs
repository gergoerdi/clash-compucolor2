{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module Hardware.Compucolor2.CRT5027 where

import Clash.Prelude

import RetroClash.Clock
import RetroClash.Port
import RetroClash.VGA
import RetroClash.Video

type TextWidth = 64
type TextHeight = 32

data CRTOut dom = CRTOut
    { crtCursor :: Signal dom (Maybe (Index TextWidth, Index TextHeight)) }

crt5027
    :: (HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Maybe (PortCommand (Index 16) (Unsigned 8)))
    -> (Signal dom (Maybe (Unsigned 8)), CRTOut dom)
crt5027 endFrame cmd = (pure $ Just 0x00, CRTOut{..})
  where
    crtCursor = pure Nothing
