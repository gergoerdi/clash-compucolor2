module Hardware.Compucolor2.Video.Plot where

import Clash.Prelude

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
