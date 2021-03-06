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

type FontWidth = 6
type FontHeight = 8

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

    extAddr' = schedule <$> hblank <*> extAddr
      where
        schedule hblank extAddr = do
            (urgent, addr) <- extAddr
            guard $ urgent || hblank
            return addr
    (extAddr1, extAddr2) = D.unbundle $ unbraid <$> extAddr'

    newChar = liftD (isRising False) $ x0 .== Just 0
    intAddr = guardA newChar $ bitCoerce <$> (liftA2 (,) <$> y1 <*> x1)

    frameBuf extAddr = sharedDelayedRW ram $
        extAddr `withWrite` extWrite :>
        noWrite intAddr :>
        Nil
      where
        ram = singlePort $ delayedRam (blockRam1 NoClearOnReset (SNat @TextSize) 0)

    extRead1 :> charRead :> Nil = frameBuf extAddr1
    extRead2 :> attrRead :> Nil = frameBuf extAddr2
    extRead = extRead1 .<|>. extRead2

    char@plotAddr = delayedRegister 0 (.|>. charRead)
    (isTall, fontAddr) = D.unbundle $ bitCoerce <$> char

    attr = delayedRegister 0 (.|>. attrRead)
    (isPlot, blink, back, fore) = D.unbundle $ bitCoerce @_ @(_, _, _, _) <$> attr

    y0' = do
        y0 <- delayI Nothing y0 .<| 0
        y1 <- delayI Nothing y1 .<| 0
        isTall <- isTall
        pure $ if isTall then bitCoerce (lsb y1, halfIndex y0) else y0

    nextBlock = enable (delayI False $ isJust <$> charRead) $
        mux (delayI False isPlot)
          (plotRom plotAddr (delayI Nothing y0 .<| 0))
          (fontRom fontAddr y0')
    block = enable (delayI False newChar) $ delayedRegister 0 (.|>. nextBlock)

    newCol = liftD (changed Nothing) x0
    pixel = liftD2 shifterL block (delayI False newCol)

    rgb = do
        x1 <- delayI Nothing x1
        y1 <- delayI Nothing y1
        cursor <- delayI Nothing $ fromSignal cursor
        blink <- delayI False blink
        pixel <- bitToBool <$> pixel
        fore <- delayI 0 fore
        back <- delayI 0 back

        pure $ case liftA2 (,) x1 y1 of
            Nothing -> (0x30, 0x30, 0x30)
            Just (x1, y1) -> fromBGR $ if pixel `xor` (isCursor || blink) then fore else back
              where
                isCursor = cursor == Just (x1', y1')
                x1' = fromIntegral @(Index TextWidth) x1
                y1' = fromIntegral @(Index TextHeight) y1

fromBGR :: (Bounded r, Bounded g, Bounded b) => Unsigned 3 -> (r, g, b)
fromBGR (bitCoerce -> (b, g, r)) = (stretch r, stretch g, stretch b)
  where
    stretch False = minBound
    stretch True = maxBound

scroll :: (SaturatingNum a) => a -> Maybe a -> Maybe a
scroll offset x = satAdd SatWrap offset <$> x

unbraid
    :: (KnownNat n, 1 <= 2 * n, (CLog 2 (2 * n)) ~ (CLog 2 n + 1))
    =>  Maybe (Index (2 * n))
    -> (Maybe (Index n), Maybe (Index n))
unbraid Nothing = (Nothing, Nothing)
unbraid (Just addr) = let (addr', sel) = bitCoerce addr in (addr' <$ guard (not sel), addr' <$ guard sel)

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
