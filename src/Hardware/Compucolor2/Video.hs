{-# LANGUAGE NumericUnderscores, RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
module Hardware.Compucolor2.Video where

import Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as D
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Delayed
import RetroClash.Clock
import Hardware.Compucolor2.CRT5027

import Control.Monad
import Data.Maybe (isJust)

type FontWidth = 6
type FontHeight = 8

type VidSize = TextWidth * TextHeight * 2
type VidAddr = Index VidSize

-- | 40 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom40", vPeriod = hzToPeriod 40_000_000}

video
    :: (HiddenClockResetEnable Dom40)
    => CRTOut Dom40
    -> Signal Dom40 (Maybe (Bool, VidAddr))
    -> Signal Dom40 (Maybe (Unsigned 8))
    -> ( VGAOut Dom40 8 8 8
       , Signal Dom40 Bool
       , Signal Dom40 (Maybe (Unsigned 8))
       )
video CRTOut{..} (unsafeFromSignal -> extAddr) (unsafeFromSignal -> extWrite) =
    ( delayVGA vgaSync rgb
    , toSignal $ delayI False frameEnd <* rgb
    , toSignal extRead
    )
  where
    VGADriver{..} = vgaDriver vga800x600at60
    -- (vgaY', scanline) = scale (SNat @2) . center $ vgaY
    (fromSignal -> textX, fromSignal -> glyphX) = scale (SNat @6) . fst . scale (SNat @2) . center $ vgaX
    (fromSignal -> textY, fromSignal -> glyphY) = scale (SNat @8) . fst . scale (SNat @2) . center $ vgaY

    frameEnd = liftD (isFalling False) (isJust <$> textY)

    rgb = do
        x <- textX
        y <- textY
        pure $ (maybe @_ @(Index TextWidth) 0 fromIntegral x, maybe @_ @(Index TextHeight) 0 fromIntegral y, 0)

    intAddr = pure Nothing

    (extAddr1, extAddr2) = D.unbundle $ unbraid <$> extAddr
    extRead1 :> vidRead :> extRead2 :> Nil = sharedDelayed (ram . D.unbundle) $
        extAddr1 `withWrite` extWrite :>
        noWrite intAddr :>
        extAddr2 `withWrite` extWrite :>
        Nil
      where
        ram (addr, wr) = delayedRam (blockRamU ClearOnReset (SNat @VidSize) (const 0)) addr (packWrite <$> addr <*> wr)

    extRead = mplus <$> extRead1 <*> extRead2

unbraid :: Maybe (Bool, a) -> (Maybe a, Maybe a)
unbraid Nothing = (Nothing, Nothing)
unbraid (Just (first, x)) = if first then (Just x, Nothing) else (Nothing, Just x)
