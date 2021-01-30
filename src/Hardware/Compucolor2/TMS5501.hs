{-# LANGUAGE ViewPatterns, LambdaCase, RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
module Hardware.Compucolor2.TMS5501
    ( Port
    , tms5501
    -- , Input(..)
    -- , Output(..)
    ) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.Port
import RetroClash.Barbies
import RetroClash.Clock

import Hardware.Intel8080 (Interrupt, Value)
import Hardware.Intel8080.Interruptor (rst)

import Control.Monad.State
import Data.Foldable (traverse_, for_)
import Data.Traversable (for)
import Data.Maybe
import Control.Lens hiding (Index, (:>))
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
    }

declareBareB [d|
  data Input = MkInput
      { senseTrigger :: Bool
      , inputTrigger :: Bool
      , parallelIn :: Value
      , ack :: Bool
      } |]

declareBareB [d|
  data Output = MkOutput
      { parallelOut :: Value
      , irq :: Bool
      , int :: Maybe Value
      } |]

tms5501
    :: (HiddenClockResetEnable dom, KnownNat (DomainPeriod dom), 1 <= DomainPeriod dom)
    => Signal dom Bit
    -> Signal dom Value
    -> Signal dom Bool
    -> Signal dom (Maybe (PortCommand Port Value))
    -> ( Signal dom (Maybe Value)
       , ( Signal dom Value
         , Signal dom Bool
         , Signal dom (Maybe Value)
         )
       )
tms5501 sense parallelIn ack cmd = (dataOut, (parallelOut, irq, int))
  where
    (dataOut, bunbundle -> MkOutput{..}) = unbundle . mealyState step initS . bundle $ (cmd, tick, bbundle MkInput{..})

    tick = risePeriod (SNat @(Microseconds 8))

    senseTrigger = isRising low sense
    inputTrigger = isRising low $ msb <$> parallelIn

    step (cmd, tick, inp@MkInput{..}) = do
        handleSense senseTrigger
        handleInput inputTrigger
        countdown tick

        pending <- getPending
        let irq = isJust pending

        (int, dataOut) <- do
            shouldAck <- use enableAck
            if shouldAck && ack then do
                traverse_ clearPending pending
                return (Just $ toRST pending, Nothing)
              else do
                dataOut <- exec inp tick cmd
                return (Nothing, Just dataOut)

        parallelOut <- complement <$> use parallelBuf
        return (dataOut, MkOutput{..})

exec :: Pure Input -> Bool -> Maybe (PortCommand Port Value) -> State S Value
exec MkInput{..} tick cmd = case cmd of
    Just (ReadPort 0x0) -> return 0x00 -- TODO: serial in
    Just (ReadPort 0x1) -> return parallelIn
    Just (ReadPort 0x2) -> do
        pending <- getPending
        traverse_ clearPending pending
        return $ toRST pending
    Just (ReadPort 0x3) -> getStatus

    Just (WritePort port x) -> (*> return 0x00) $ case port of
        0x4 -> execDiscrete x
        0x5 -> return () -- TODO: UART set baud rate
        0x6 -> return () -- TODO: UAT serial out
        0x7 -> parallelBuf .= x
        0x8 -> intMask .= x
        0x9 -> setTimer 0 x
        0xa -> setTimer 1 x
        0xb -> setTimer 2 x
        0xc -> setTimer 3 x
        0xd -> setTimer 4 x
        _ -> return ()

    _ -> return 0x00

countdown :: Bool -> State S ()
countdown tick = do
    tick' <- do
        scale <- use tickScaler
        case scale of
            Nothing -> return tick
            Just cnt -> do
                when tick $ tickScaler .= Just (prevIdx cnt)
                return $ tick && cnt == 0

    when tick' $ forM_ [0..4] $ \(fromIntegral -> i) -> do
        count <- uses timers (!! i)
        traverse_ (setTimer i) (predIdx count)

handleSense :: Bool -> State S ()
handleSense trigger = when trigger $ setInt 2

handleInput :: Bool -> State S ()
handleInput trigger = when trigger $ do
    enabled <- use enableInputTrigger
    when enabled $ setInt 7

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
            4 | enableTimer4 -> Just 7
            _ -> Nothing

getStatus :: State S Value
getStatus = do
    intPending <- isJust <$> getPending
    return $ bitCoerce $
      False :>      -- TODO: UART start bit detected
      False :>      -- TODO: UART first data bit detected
      intPending :>
      True :>       -- TODO: UART ready to transmit
      False :>      -- TODO: UART receiver buffer loaded
      True :>       -- TODO: UART receiver line monitor
      False :>      -- TODO: UART overrun -- TODO: reset on getStatus
      False :>      -- TODO: UART frame error (stop bit in error)
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
    -- reset takes precedence over break
    when (cmd `testBit` 1) break
    when (cmd `testBit` 0) reset

    enableInputTrigger .= cmd `testBit` 2
    enableAck .= cmd `testBit` 3
    tickScaler .= if cmd `testBit` 4 then Nothing else Just maxBound
    -- TODO: bit 5 --> interrupt output is clock (?)
  where
    break = do
        -- TODO
        return ()

    reset = do
        -- TODO: reset UART
        intBuf .= 0b0001_0000
        timers .= repeat 0
