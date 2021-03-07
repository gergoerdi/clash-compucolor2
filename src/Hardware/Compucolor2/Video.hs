{-# LANGUAGE NumericUnderscores, RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
module Hardware.Compucolor2.Video
    ( Dom40
    , FontWidth
    , FontHeight
    , VidAddr
    , video
    ) where

import Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as D
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Delayed hiding (delayedBlockRam1)
import RetroClash.Barbies

import Hardware.Compucolor2.CRT5027 as CRT5027
import Hardware.Compucolor2.Video.Plot

import Control.Monad
import Data.Maybe (isJust, isNothing, fromMaybe)
import qualified Language.Haskell.TH.Syntax as TH

type TextSize = TextWidth * TextHeight
type VidSize = TextSize * 2
type VidAddr = Index VidSize

-- | 40 MHz clock, needed for the VGA mode we use.
createDomain vSystem{vName="Dom40", vPeriod = hzToPeriod 40_000_000}

video
    :: (HiddenClockResetEnable Dom40)
    => Signals Dom40 CRT5027.Output
    -> Signal Dom40 (Maybe (Bool, VidAddr))
    -> Signal Dom40 (Maybe (Unsigned 8))
    -> ( VGAOut Dom40 8 8 8
       , Signal Dom40 Bool
       , Signal Dom40 (Maybe (Unsigned 8))
       )
video CRT5027.MkOutput{..} (unsafeFromSignal -> extAddr) (unsafeFromSignal -> extWrite) =
    ( delayVGA vgaSync rgb
    , toSignal $ delayI False frameEnd <* rgb
    , toSignal extRead
    )
  where
    VGADriver{..} = vgaDriver vga800x600at60
    (fromSignal -> x1, fromSignal -> x0) = scale (SNat @FontWidth) . fst . scale (SNat @2) . center $ vgaX
    (fromSignal -> rawY1, fromSignal -> y0) = scale (SNat @FontHeight) . fst . scale (SNat @2) . center $ vgaY
    y1 = scroll <$> fromSignal scrollOffset <*> rawY1

    vblank = isNothing <$> y1
    hblank = isNothing <$> x1
    frameEnd = liftD (isRising False) vblank

    newCol = liftD (changed Nothing) x0
    newChar = liftD (changed Nothing) x1
    visible = isJust <$> x1 .&&. isJust <$> y1

    extAddr' = schedule <$> hblank <*> extAddr
      where
        schedule hblank extAddr = do
            (urgent, addr) <- extAddr
            guard $ urgent || hblank
            return addr
    extAddr1 :> extAddr2 :> Nil = D.unbundle $ unbraid <$> extAddr'

    intAddr = guardA newChar $ liftA2 toAddr <$> x1 <*> y1
      where
        toAddr :: Index TextWidth -> Index TextHeight -> Index TextSize
        toAddr x1 y1 = bitCoerce (y1, x1)

    videoMem extAddr = sharedDelayedRW ram $
        extAddr `withWrite` extWrite :>
        noWrite intAddr :>
        Nil
      where
        ram = singlePort $ delayedRam (blockRam1 NoClearOnReset (SNat @TextSize) 0)

    extRead1 :> charRead :> Nil = videoMem extAddr1
    extRead2 :> attrRead :> Nil = videoMem extAddr2
    extRead = extRead1 .<|>. extRead2

    char@plotAddr = charRead .<| 0
    (isTall, fontAddr) = D.unbundle $ bitCoerce <$> char

    attr = delayedRegister 0 (.|>. attrRead)
    (isPlot, blink, back, fore) = D.unbundle $ attributes <$> attr

    y0' = mux isTall tall short .<| 0
      where
        short = delayI Nothing y0
        tall = delayI Nothing $ liftA2 toTall <$> y1 <*> y0

    block = enable (delayI False newChar) $
        mux (delayI False isPlot)
          (plotRom plotAddr (delayI Nothing y0 .<| 0))
          (fontRom fontAddr y0')
    pixel = liftD2 shifterL block (delayI False newCol)

    border = (0x30, 0x30, 0x30)
    white = pure (1, 1, 1)
    black = pure (0, 0, 0)

    palette = D.bundle $
        back :>
        mux blinked black fore :>
        Nil

    (blinked, isCursor) = D.unbundle $ do
        cursor <- delayI Nothing $ fromSignal cursor
        x1 <- delayI Nothing x1 .<| 0
        y1 <- delayI Nothing y1 .<| 0
        y0 <- delayI Nothing y0
        blink <- blink
        pure $
            let atCursor = cursor == Just (fromIntegral x1, fromIntegral y1)
                cursorRow = any (y0 ==) [Just minBound, Just maxBound]
            in (blink && isJust cursor, atCursor && cursorRow)

    rgb =
        mux (not <$> delayI False visible) (pure border) $ fmap fromBGR $
        mux isCursor white $
        palette .!!. pixel

scroll :: (SaturatingNum a) => a -> Maybe a -> Maybe a
scroll offset x = satAdd SatWrap offset <$> x

fontRom
    :: (HiddenClockResetEnable dom)
    => DSignal dom n (Unsigned 7)
    -> DSignal dom n (Index FontHeight)
    -> DSignal dom (n + 1) (BitVector 8)
fontRom char row = delayedRom (romFilePow2 "_build/chargen.uf6.bin") $
    bitCoerce <$> D.bundle (char, row)

plotRom
    :: (HiddenClockResetEnable dom)
    => DSignal dom n (Unsigned 8)
    -> DSignal dom n (Index 8)
    -> DSignal dom (n + 1) (BitVector 8)
plotRom char row = stretchRow <$> b1 <*> b2
  where
    (hi, lo) = D.unbundle . fmap splitChar $ char
    row' = halfIndex <$> row

    b1 = delayedRom (rom $(TH.lift plots)) $ plotAddr <$> lo <*> row'
    b2 = delayedRom (rom $(TH.lift plots)) $ plotAddr <$> hi <*> row'
