{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
module Hardware.Compucolor2.TMS5501
    ( Ctl.Port
    , Output(..)
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

import Barbies.TH
import Data.Tuple.Curry

declareBareB [d|
  data Output = MkOutput
      { parallelOut :: Value
      , serialOut :: Bit
      , interruptRequest :: Bool
      , rst :: Maybe Value
      } |]

tms5501
    :: (HiddenClockResetEnable dom, KnownNat (DomainPeriod dom), 1 <= DomainPeriod dom)
    => Signal dom Bool
    -> Signal dom Value
    -> Signal dom Bit
    -> Signal dom Bool
    -> Signal dom (Maybe (PortCommand Ctl.Port Value))
    -> ( Signal dom (Maybe Value)
       , Signals dom Output
       )
tms5501 sense parallelIn serialIn ack cmd = (dataOut, out)
  where
    out = MkOutput{..}

    (dataOut, bunbundle -> Ctl.MkOutput{..}) = mealyStateB (uncurryN Ctl.controller) Ctl.initS (bbundle Ctl.MkInput{..}, tick, cmd)
    interruptRequest = delay False irq
    rst = delay Nothing int

    tick = risePeriod (SNat @(Microseconds 8))

    senseTrigger = isRising False sense
    inputTrigger = isRising low $ msb <$> parallelIn

    (rxResult, bunbundle -> UART.MkOutput{..}) = mealyStateB (uncurryN UART.uart) UART.initS (bbundle UART.MkInput{..}, register Nothing txNew)
