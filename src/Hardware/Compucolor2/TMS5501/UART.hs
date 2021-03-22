{-# LANGUAGE LambdaCase, RecordWildCards #-}
module Hardware.Compucolor2.TMS5501.UART
    ( uart
    , initS
    , Input(..)
    , Output(..)
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

type Port = Index 16

data S = MkS
    { _txState :: TxState 8
    , _rxState :: RxState 8
    , _rxStart :: Bool
    , _rxData :: Bool
    , _rxFrameError :: Bool
    }
    deriving (Show, Generic, NFDataX)
makeLenses ''S

initS :: S
initS = MkS
    { _txState = TxIdle
    , _rxState = RxIdle
    , _rxStart = False
    , _rxData = False
    , _rxFrameError = False
    }

declareBareB [d|
  data Input = MkInput
      { serialIn :: Bit
      } |]

declareBareB [d|
  data Output = MkOutput
      { serialOut :: Bit
      , txReady :: Bool
      , rxInfo :: (Bool, Bool, Bool)
      } |]

uart :: Pure Input -> Maybe (Unsigned 8) -> State S (Maybe (Unsigned 8), Pure Output)
uart MkInput{..} newTx = do
    -- TODO: calculate bitDuration from clock, aiming for 20 * 8 * 9600 bps
    -- TODO: 4166 => 9600 bps
    -- TODO: 26 => 20*8*9600 bps
    let bitDuration = 26
    (serialOut, txReady) <- zoom txState $ txStep bitDuration (pack <$> newTx)
    rxResult <- zoom rxState $ rxStep bitDuration serialIn
    rxInfo <- updateRxState
    return (unpack <$> rxResult, MkOutput{..})

updateRxState :: State S (Bool, Bool, Bool)
updateRxState = do
    use rxState >>= \case
        RxBit _ (Just 1) Rx.StartBit{} -> do
            rxStart .= True
        RxBit _ (Just _) Rx.DataBit{} -> do
            rxData .= True
        RxBit _ (Just sample) Rx.StopBit{} -> do
            rxStart .= False
            rxData .= False
            rxFrameError .= not (sample == high)
        _ -> return ()

    rxStart <- use rxStart
    rxData <- use rxData
    rxFrameError <- use rxFrameError
    return (rxStart, rxData, rxFrameError)
