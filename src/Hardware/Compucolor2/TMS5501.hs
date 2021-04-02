{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
module Hardware.Compucolor2.TMS5501
    ( Ctl.Port
    , Input(..)
    , Output(..)
    , tms5501
    , UART.SlowRate
    , UART.FastRate
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
  data Input = MkInput
      { parallelIn :: BitVector 8
      , sensor :: Bit
      , serialIn :: Bit
      , ack :: Bool
      , turbo :: Bool
      } |]

declareBareB [d|
  data Output = MkOutput
      { parallelOut :: BitVector 8
      , serialOut :: Bit
      , interruptRequest :: Bool
      , rst :: Maybe Value
      } |]

tms5501
    :: forall dom. (HiddenClockResetEnable dom, KnownNat (DomainPeriod dom), 1 <= DomainPeriod dom)
    => Signals dom Input
    -> Signal dom (Maybe (PortCommand Ctl.Port Value))
    -> ( Signal dom (Maybe Value)
       , Signals dom Output
       )
tms5501 MkInput{..} cmd = (dataOut, out)
  where
    out = MkOutput{..}

    (dataOut, unbundle -> Ctl.MkOutput{..}) =
        mealyStateB (uncurryN Ctl.controller) Ctl.initS (bbundle Ctl.MkInput{..}, cmd)
    interruptRequest = delay False irq
    rst = delay Nothing int

    fastTick = risePeriod (SNat @(Microseconds 8))
    slowTick = riseEveryWhen (SNat @8) fastTick
    tick = mux (delay False fast) fastTick slowTick

    sensorTrigger = isRising low sensor
    inputTrigger = isRising low $ msb <$> parallelIn

    (rxResult, rxFlags) =
        mealyStateB (uncurryN $ UART.uartRx (SNat @(DomainPeriod dom))) UART.initRxS (fast, turbo, serialIn, rxReset)

    (serialOut, txReady) =
        mealyStateB (uncurryN $ UART.uartTx (SNat @(DomainPeriod dom))) UART.initTxS (fast, turbo, txNew, txBreak)
