{-# LANGUAGE ViewPatterns, RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
module Hardware.Compucolor2.TMS5501.Controller
    ( Port
    , initS
    , controller
    , Input(..)
    , Output(..)
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Port
import RetroClash.Barbies

import Hardware.Intel8080 (Interrupt, Value)
import Hardware.Intel8080.Interruptor (rst)

import Control.Monad.State
import Data.Foldable (traverse_, for_)
import Data.Maybe
import Control.Lens hiding (Index, (:>))
import Control.Monad.Extra
import Barbies.TH

type Port = Index 16

data S = MkS
    { _timers :: Vec 5 Value
    , _intBuf :: Value
    , _intMask :: Value
    , _parallelBuf :: Value
    , _enableInputTrigger :: Bool
    , _enableAck :: Bool
    , _tickScaler :: Maybe (Index 8)
    , _rxBuf :: Unsigned 8
    , _rxReady :: Bool
    , _rxOverrun :: Bool
    , _txBuf :: Maybe (Unsigned 8)
    }
    deriving (Show, Generic, NFDataX)
makeLenses ''S

initS :: S
initS = MkS
    { _timers = repeat 0
    , _intBuf = 0x00
    , _intMask = 0x00
    , _parallelBuf = 0x00
    , _enableInputTrigger = False
    , _enableAck = True
    , _tickScaler = Just maxBound
    , _rxBuf = 0x00
    , _rxReady = False
    , _rxOverrun = False
    , _txBuf = Nothing
    }

declareBareB [d|
  data Input = MkInput
      { senseTrigger :: Bool
      , inputTrigger :: Bool
      , parallelIn :: Value
      , serialIn :: Bit
      , txReady :: Bool
      , rxResult :: Maybe (Unsigned 8)
      , rxInfo :: (Bool, Bool, Bool)
      , ack :: Bool
      } |]

declareBareB [d|
  data Output = MkOutput
      { parallelOut :: Value
      , txNew :: Maybe (Unsigned 8)
      , irq :: Bool
      , int :: Maybe Value
      } |]

controller :: Pure Input -> Bool -> Maybe (PortCommand Port Value) -> State S (Maybe Value, Pure Output)
controller inp@MkInput{..} tick cmd = do
    when senseTrigger $ setInt 2
    when inputTrigger $ do
        enabled <- use enableInputTrigger
        when enabled $ setInt 7
    countdown tick

    txNew <- use txBuf

    when txReady $ do
        whenM (isJust <$> use txBuf) $ setInt 5
        txBuf .= Nothing

    for_ rxResult $ \x -> do
        whenM (use rxReady) $ rxOverrun .= True
        setInt 4
        rxReady .= True
        rxBuf .= x

    (int, dataOut) <- do
        shouldAck <- use enableAck
        if shouldAck && ack then do
            pending <- getPending
            traverse_ clearPending pending
            return (Just $ toRST pending, Nothing)
          else do
            dataOut <- traverse (exec inp tick) cmd
            return (Nothing, dataOut)

    -- This is after handling the `ack`, since it could have cleared the previously pending irq
    irq <- isJust <$> getPending

    parallelOut <- complement <$> use parallelBuf
    return (dataOut, MkOutput{..})

exec :: Pure Input -> Bool -> PortCommand Port Value -> State S Value
exec inp@MkInput{..} tick cmd = case cmd of
    ReadPort 0x0 -> do
        rxReady .= False
        use rxBuf
    ReadPort 0x1 -> return parallelIn
    ReadPort 0x2 -> do
        pending <- getPending
        traverse_ clearPending pending
        return $ toRST pending
    ReadPort 0x3 -> getStatus inp

    WritePort port x -> (*> return 0x00) $ case port of
        0x4 -> execDiscrete x
        0x5 -> return () -- TODO: UART set baud rate
        0x6 -> txBuf .= Just x
        0x7 -> parallelBuf .= x
        0x8 -> intMask .= x
        0x9 -> setTimer 0 x
        0xa -> setTimer 1 x
        0xb -> setTimer 2 x
        0xc -> setTimer 3 x
        0xd -> setTimer 4 x
        _ -> return ()

countdown :: Bool -> State S ()
countdown tick = do
    tick' <- do
        scale <- use tickScaler
        case scale of
            Nothing -> return tick
            Just cnt -> do
                when tick $ tickScaler .= Just (prevIdx cnt)
                return $ tick && cnt == 0

    when tick' $ for_ [0..4] $ \(fromIntegral -> i) -> do
        count <- uses timers (!! i)
        traverse_ (setTimer i) (predIdx count)

setInt :: Index 8 -> State S ()
setInt i = intBuf %= (`setBit` fromIntegral i)

setTimer :: Index 5 -> Value -> State S ()
setTimer i newCount = do
    timers %= replace i newCount
    when (newCount == 0) $ do
        enableTimer4 <- not <$> use enableInputTrigger
        traverse_ setInt $ case i of
            0 -> Just 0
            1 -> Just 1
            2 -> Just 3
            3 -> Just 6
            4 -> guard (enableTimer4) >> Just 7

getStatus :: Pure Input -> State S Value
getStatus MkInput{..} = do
    intPending <- isJust <$> getPending
    rxReady <- use rxReady
    rxOverrun <- use rxOverrun <* (rxOverrun .= False)
    let (rxStart, rxData, rxFrameError) = rxInfo

    return $ bitCoerce $
      rxStart :>
      rxData :>
      intPending :>
      txReady :>
      rxReady :>
      bitToBool serialIn :>
      rxOverrun :>
      rxFrameError :>
      Nil

getMaskedInterrupt :: State S Value
getMaskedInterrupt = maskBy <$> use intMask <*> use intBuf
  where
    maskBy mask = (mask .&.)

getPending :: State S (Maybe Interrupt)
getPending = do
    masked <- getMaskedInterrupt
    return $ if masked == 0 then Nothing else Just . fromIntegral $ countTrailingZeros masked

toRST :: Maybe Interrupt -> Value
toRST = rst . fromMaybe 7

clearPending :: Interrupt -> State S ()
clearPending i = do
    intBuf %= (`clearBit` fromIntegral i)

execDiscrete :: Value -> State S ()
execDiscrete cmd = do
    when (cmd `testBit` 0) reset
    when (cmd `testBit` 1) break

    enableInputTrigger .= cmd `testBit` 2
    enableAck .= cmd `testBit` 3
    tickScaler .= if cmd `testBit` 4 then Nothing else Just maxBound
    -- TODO: bit 5 --> interrupt output is clock (?)
  where
    reset = do
        intBuf .= 0b0001_0000
        timers .= repeat 0

        -- rxStart .= False
        -- rxData .= False
        -- txState .= TxIdle
        rxReady .= False
        rxOverrun .= False

    break = do
        -- txState .= TxIdle
        return ()
