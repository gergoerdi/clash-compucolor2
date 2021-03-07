module Hardware.Compucolor2.Video.Plot where

import Clash.Prelude
import RetroClash.Utils (halfIndex)
import Hardware.Compucolor2.CRT5027 (TextWidth, TextHeight)

type FontWidth = 6
type FontHeight = 8

splitChar :: Unsigned 8 -> (Index 16, Index 16)
splitChar = bitCoerce

rowsOf :: Index 16 -> Vec 4 Bit
rowsOf = reverse . bitCoerce

plots :: Vec (16 * 4) Bit
plots = concatMap rowsOf indicesI

stretchRow :: Bit -> Bit -> BitVector 8
stretchRow b1 b2 = bitCoerce $
    replicate (SNat @3) b1 ++
    replicate (SNat @3) b2 ++
    repeat 0

plotAddr :: Index 16 -> Index 4 -> Unsigned 6
plotAddr halfChar row = bitCoerce (halfChar, row)

toTall :: Index TextHeight -> Index FontHeight -> Index FontHeight
toTall y1 y0 = bitCoerce (lsb y1, halfIndex y0)

attributes :: Unsigned 8 -> (Bool, Bool, (Bit, Bit, Bit), (Bit, Bit, Bit))
attributes = bitCoerce

fromBGR :: (Bounded r, Bounded g, Bounded b) => (Bit, Bit, Bit) -> (r, g, b)
fromBGR (b, g, r) = (stretch r, stretch g, stretch b)
  where
    stretch 0 = minBound
    stretch 1 = maxBound
