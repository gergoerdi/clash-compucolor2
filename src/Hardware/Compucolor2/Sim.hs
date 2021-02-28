{-# LANGUAGE OverloadedStrings #-}
module Hardware.Compucolor2.Sim where

import Clash.Prelude
import Clash.Sized.Internal.BitVector

divI
    :: (KnownNat n, KnownNat k, 1 <= k, n ~ ((n `Div` k) * k))
    => SNat k
    -> Index n
    -> (Index (n `Div` k), Index k)
divI k@SNat x = (fromIntegral x1, fromIntegral x0)
  where
    (x1, x0) = x `quotRem` snatToNum k

toFFI :: (BitPack a, BitPack b, KnownNat n, BitSize b ~ (BitSize a + n)) => a -> b
toFFI = unpack . ensureBits . resize . pack

ensureBits :: BitVector n -> BitVector n
ensureBits (BV _mask bs) = BV 0 bs

fromFFI :: (BitPack a, BitPack b, KnownNat n, BitSize b ~ (BitSize a + n)) => b -> a
fromFFI = unpack . resize . pack
