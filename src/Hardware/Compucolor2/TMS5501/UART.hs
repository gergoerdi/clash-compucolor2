{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Hardware.Compucolor2.TMS5501.UART
    ( SlowRate
    , FastRate
    , uartRx
    , initRxS
    , uartTx
    , initTxS
    , RxFlags(..)
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Barbies
import RetroClash.Clock
import RetroClash.SerialTx as Tx
import RetroClash.SerialRx as Rx

import Control.Monad.State
import Control.Lens hiding (Index, (:>))
import Barbies.TH
import Data.Tuple.Curry

type SlowRate = 9600
type FastRate = SlowRate * 8

data RxFlags = RxFlags
    { _rxStart :: Bool
    , _rxData :: Bool
    , _rxFrameError :: Bool
    }
    deriving (Show, Generic, NFDataX)
makeLenses ''RxFlags

data RxS = MkRxS
    { _rxState :: RxState 8
    , _rxFlags :: RxFlags
    }
    deriving (Show, Generic, NFDataX)
makeLenses ''RxS

initRxS :: RxS
initRxS = MkRxS
    { _rxState = RxIdle
    , _rxFlags = RxFlags False False False
    }

uartRx
    :: forall period. (KnownNat period, 1 <= period)
    => SNat period
    -> Bit
    -> Bool
    -> State RxS (Maybe (Unsigned 8), RxFlags)
uartRx period serialIn reset = do
    rxResult <-
        if reset then do
            rxFlags .= RxFlags False False False
            rxState .= RxIdle
            return Nothing
        else do
            rxResult <- zoom rxState $ rxStep bitDuration serialIn
            rxState <- use rxState
            zoom rxFlags $ updateRxFlags rxState
            return rxResult
    rxFlags <- use rxFlags
    return (unpack <$> rxResult, rxFlags)
  where
    bitDuration = snatToNum $ SNat @(HzToPeriod FastRate `Div` period)

type TxS = TxState 8

initTxS :: TxS
initTxS = TxIdle

uartTx
    :: forall period. (KnownNat period, 1 <= period)
    => SNat period
    -> Maybe (Unsigned 8)
    -> Bool
    -> State TxS (Bit, Bool)
uartTx period newTx break = do
    when break $ put TxIdle
    txStep bitDuration (pack <$> newTx)
  where
    bitDuration = snatToNum $ SNat @(HzToPeriod FastRate `Div` period)

updateRxFlags :: RxState n -> State RxFlags ()
updateRxFlags = \case
    RxBit _ (Just 1) Rx.StartBit{} -> do
        rxStart .= True
    RxBit _ (Just _) Rx.DataBit{} -> do
        rxData .= True
    RxBit _ (Just sample) Rx.StopBit{} -> do
        rxStart .= False
        rxData .= False
        rxFrameError .= (sample /= high)
    _ -> return ()
