module Hardware.Compucolor2.Video.Plot where

import Clash.Prelude

type FontWidth = 6
type FontHeight = 8

rowsOf :: Index 16 -> Vec (FontHeight `Div` 2) Bit
rowsOf = reverse . bitCoerce

plots :: Vec (16 * (FontHeight `Div` 2)) Bit
plots = concatMap rowsOf indicesI

stretchRow :: Bit -> Bit -> Unsigned 8
stretchRow b1 b2 = bitCoerce $
    replicate (SNat @3) b1 ++
    replicate (SNat @3) b2 ++
    repeat 0
