{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
module Hardware.Compucolor2.TMS5501
    ( Ctl.Port
    , tms5501
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Port
import RetroClash.Barbies
import RetroClash.Clock
import Hardware.Intel8080 (Value)

import qualified Hardware.Compucolor2.TMS5501.UART as UART
import qualified Hardware.Compucolor2.TMS5501.Controller as Ctl

import Data.Tuple.Curry

tms5501
    :: (HiddenClockResetEnable dom, KnownNat (DomainPeriod dom), 1 <= DomainPeriod dom)
    => Signal dom Bool
    -> Signal dom Value
    -> Signal dom Bit
    -> Signal dom Bool
    -> Signal dom (Maybe (PortCommand Ctl.Port Value))
    -> ( Signal dom (Maybe Value)
       , ( Signal dom Value
         , Signal dom Bit
         , Signal dom Bool
         , Signal dom (Maybe Value)
         )
       )
tms5501 sense parallelIn serialIn ack cmd = (dataOut, (parallelOut, serialOut, delay False irq, delay Nothing int))
  where
    (dataOut, bunbundle -> Ctl.MkOutput{..}) = mealyStateB (uncurryN Ctl.controller) Ctl.initS (bbundle Ctl.MkInput{..}, tick, cmd)

    tick = risePeriod (SNat @(Microseconds 8))

    senseTrigger = isRising False sense
    inputTrigger = isRising low $ msb <$> parallelIn

    (rxResult, bunbundle -> UART.MkOutput{..}) = mealyStateB (uncurryN UART.uart) UART.initS (bbundle UART.MkInput{..}, register Nothing txNew)
