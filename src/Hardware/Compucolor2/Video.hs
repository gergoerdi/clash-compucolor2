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
    (fromSignal -> textX, fromSignal -> glyphX) = scale (SNat @FontWidth) . fst . scale (SNat @2) . center $ vgaX
    (fromSignal -> textY0, fromSignal -> glyphY) = scale (SNat @FontHeight) . fst . scale (SNat @2) . center $ vgaY
    textY = scroll <$> fromSignal scrollOffset <*> textY0

    vblank = isNothing <$> textY
    hblank = isNothing <$> textX
    frameEnd = liftD (isRising False) vblank

    extAddr' = schedule <$> hblank <*> extAddr
      where
        schedule hblank extAddr = do
            (urgent, addr) <- extAddr
            guard $ urgent || hblank
            return addr
    (extAddr1, extAddr2) = D.unbundle $ unbraid <$> extAddr'

    newChar = liftD (isRising False) $ glyphX .== Just 0
    intAddr = guardA newChar $ bitCoerce <$> (liftA2 (,) <$> textY <*> textX)

    frameBuf extAddr = sharedDelayedRW ram $
        extAddr `withWrite` extWrite :>
        noWrite intAddr :>
        Nil
      where
        ram = singlePort $ delayedRam (blockRam1 NoClearOnReset (SNat @TextSize) 0)

    extRead1 :> charRead :> Nil = frameBuf extAddr1
    extRead2 :> attrRead :> Nil = frameBuf extAddr2
    extRead = extRead1 .<|>. extRead2

    char = delayedRegister 0 (.|>. charRead)
    (isTall, glyphAddr) = D.unbundle $ bitCoerce <$> char

    attr = delayedRegister 0 (.|>. attrRead)
    (isPlot, blink, back, fore) = D.unbundle $ bitCoerce @_ @(_, _, _, _) <$> attr

    glyphY' = do
        y <- delayI Nothing glyphY .<| 0
        ty <- delayI Nothing textY .<| 0
        isTall <- isTall
        pure $ if isTall then half y + (if odd ty then 4 else 0) else y

    nextBlock = enable (delayI False $ isJust <$> charRead) $
        mux (delayI False isPlot)
          (plotRom char (delayI Nothing glyphY .<| 0))
          (fontRom glyphAddr glyphY')
    block = enable (delayI False newChar) $ delayedRegister 0 (.|>. nextBlock)

    newCol = liftD (changed Nothing) glyphX
    pixel = liftD2 shifterL block (delayI False newCol)

    rgb = do
        x <- delayI Nothing textX
        y <- delayI Nothing textY
        cursor <- delayI Nothing $ fromSignal cursor
        blink <- delayI False blink
        pixel <- bitToBool <$> pixel
        fore <- delayI 0 fore
        back <- delayI 0 back

        pure $ case liftA2 (,) x y of
            Nothing -> (0x30, 0x30, 0x30)
            Just (x, y) -> fromBGR $ if pixel `xor` (isCursor || blink) then fore else back
              where
                isCursor = cursor == Just (x', y')
                x' = fromIntegral @(Index TextWidth) x
                y' = fromIntegral @(Index TextHeight) y

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
    -> DSignal dom (n + 1) (Unsigned 8)
fontRom char row = delayedRom (fmap unpack . romFilePow2 "_build/chargen.uf6.bin") $
    toAddr <$> char <*> row
  where
    toAddr :: Unsigned 7 -> Index 8 -> Unsigned (7 + CLog 2 FontHeight)
    toAddr char row = bitCoerce (char, row)

plotRom
    :: (HiddenClockResetEnable dom)
    => DSignal dom n (Unsigned 8)
    -> DSignal dom n (Index FontHeight)
    -> DSignal dom (n + 1) (Unsigned 8)
plotRom char row = stretchRow <$> col1 <*> col2
  where
    (char2, char1) = D.unbundle . fmap bitCoerce $ char

    col1 = delayedRom (rom $(TH.lift plots)) $ toAddr <$> char1 <*> row
    col2 = delayedRom (rom $(TH.lift plots)) $ toAddr <$> char2 <*> row

    toAddr :: Unsigned 4 -> Index 8 -> Unsigned (4 + CLog 2 (FontHeight `Div` 2))
    toAddr char row = bitCoerce (char, row')
      where
        (row', _) = bitCoerce @_ @(Index (FontHeight `Div` 2), Bit) row
