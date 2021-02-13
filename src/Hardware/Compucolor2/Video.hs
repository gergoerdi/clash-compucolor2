{-# LANGUAGE NumericUnderscores, RecordWildCards, ViewPatterns #-}
{-# LANGUAGE ApplicativeDo #-}
module Hardware.Compucolor2.Video where

import Clash.Prelude
import qualified Clash.Signal.Delayed.Bundle as D
import RetroClash.Utils
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Delayed
import RetroClash.Barbies

import Hardware.Compucolor2.CRT5027 as CRT5027

import Control.Monad
import Data.Maybe (isJust, fromMaybe)

type FontWidth = 6
type FontHeight = 8

type VidSize = TextWidth * TextHeight * 2
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
    -- (vgaY', scanline) = scale (SNat @2) . center $ vgaY
    (fromSignal -> textX, fromSignal -> glyphX) = scale (SNat @6) . fst . scale (SNat @2) . center $ vgaX
    (fromSignal -> textY0, fromSignal -> glyphY) = scale (SNat @8) . fst . scale (SNat @2) . center $ vgaY
    textY = do
        offset <- fromSignal scrollOffset
        y0 <- textY0
        pure $ satAdd SatWrap offset <$> y0

    frameEnd = liftD (isFalling False) (isJust <$> textY)

    newChar = liftD (isRising False) $ glyphX .== Just 0

    (charAddr, attrAddr) = (redelayI charAddr0, fmap (+1) <$> charAddr1)
      where
        charAddr0 = do
            x <- textX
            y <- textY
            newChar <- newChar
            pure $ case (x, y, newChar) of
                (Just x, Just y, True) -> Just $ bitCoerce (y, x, (0 :: Index 2))
                _ -> Nothing

        charAddr1 = delayN (SNat @1) Nothing charAddr0

    intAddr = muxA [charAddr, attrAddr]

    (extAddr1, extAddr2) = D.unbundle $ unbraid <$> extAddr
    extRead1 :> intLoad :> extRead2 :> Nil = sharedDelayed (ram . D.unbundle . fmap (fromMaybe (0, Nothing))) $
        extAddr1 `withWrite` extWrite :>
        noWrite intAddr :>
        extAddr2 `withWrite` extWrite :>
        Nil
      where
        ram (addr, wr) = delayedRam (blockRam1 NoClearOnReset (SNat @VidSize) 0) addr (packWrite <$> addr <*> wr)

    extRead = mplus <$> extRead1 <*> extRead2

    charLoad = guardA (isJust <$> delayI Nothing charAddr) intLoad
    (isTall, glyphAddr) = D.unbundle $ bitCoerce . fromMaybe 0 <$> charLoad

    attrLoad = guardA (isJust <$> delayI Nothing attrAddr) intLoad
    attr = delayedRegister 0x00 (attrLoad .|>.)
    (isPlot, blink, back, fore) = D.unbundle $ bitCoerce @_ @(_, Bool, Unsigned 3, Unsigned 3) <$> attr

    glyphY' = do
        y <- delayI Nothing glyphY .|>. 0
        ty <- delayI Nothing textY .|>. 0
        isTall <- isTall
        pure $ let y' = y `shiftR` 1 + if odd ty then 4 else 0
               in if isTall then y' else y

    glyphLoad = enable (delayI False $ isJust <$> charLoad) $
        mux (delayI False isPlot)
          (fontRom glyphAddr glyphY') -- TODO: get glyph data from char itself
          (fontRom glyphAddr glyphY')
    newCol = liftD (changed Nothing) glyphX

    pixel = liftD2 shifterL glyphLoad (delayI False newCol)

    rgb = do
        x <- delayI Nothing textX
        y <- delayI Nothing textY
        cursor <- delayI Nothing $ fromSignal cursor
        pixel <- bitToBool <$> pixel
        fore <- delayI 0 fore
        back <- delayI 0 back

        pure $ case liftA2 (,) x y of
            Nothing -> (0x30, 0x30, 0x30)
            Just (x, y) -> fromBGR $ if pixel `xor` isCursor then fore else back
              where
                isCursor = cursor == Just (x', y')
                x' = fromIntegral @(Index TextWidth) x
                y' = fromIntegral @(Index TextHeight) y

fromBGR :: (Bounded r, Bounded g, Bounded b) => Unsigned 3 -> (r, g, b)
fromBGR (bitCoerce -> (b, g, r)) = (stretch r, stretch g, stretch b)
  where
    stretch False = minBound
    stretch True = maxBound

unbraid :: Maybe (Bool, a) -> (Maybe a, Maybe a)
unbraid Nothing = (Nothing, Nothing)
unbraid (Just (first, x)) = if first then (Just x, Nothing) else (Nothing, Just x)

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

redelayI :: (KnownNat k, HiddenClockResetEnable dom) => DSignal dom d a -> DSignal dom (d+k) a
redelayI = unsafeFromSignal . toSignal

redelayN :: (HiddenClockResetEnable dom) => SNat k -> DSignal dom d a -> DSignal dom (d+k) a
redelayN SNat = redelayI
