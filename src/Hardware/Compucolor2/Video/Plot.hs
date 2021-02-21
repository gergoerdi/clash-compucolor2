module Hardware.Compucolor2.Video.Plot where

import Clash.Prelude

type FontWidth = 6
type FontHeight = 8

rowsOf :: Index 16 -> Vec (FontHeight `Div` 2) Bit
rowsOf char =
    char!0 :>
    char!1 :>
    char!2 :>
    char!3 :>
    Nil

plots :: Vec (16 * (FontHeight `Div` 2)) Bit
plots = concatMap rowsOf indicesI

stretchRow :: Bit -> Bit -> Unsigned 8
stretchRow col1 col2 = bitCoerce $ col1 :> col1 :> col1 :> col2 :> col2 :> col2 :> repeat 0
