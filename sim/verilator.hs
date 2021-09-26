{-# LANGUAGE OverloadedStrings, RecordWildCards, NumericUnderscores #-}
{-# OPTIONS_GHC -Wunused-imports #-}
import Clash.Prelude

import Clash.Clashilator.FFI
import Foreign.Storable
import Foreign.Marshal.Alloc

{-# INLINE withRunner #-}
withRunner :: ((INPUT -> IO OUTPUT) -> IO a) -> IO a
withRunner act = alloca $ \inp -> alloca $ \outp -> do
    sim <- simInit
    let step input = do
            poke inp input
            simStep sim inp outp
            peek outp
    x <- act step
    simShutdown sim
    return x

main :: IO ()
main = withRunner $ \runCycle -> do
    let input = INPUT
            { iRESET = low
            }

    OUTPUT{..} <- runCycle input
    print oVGA
