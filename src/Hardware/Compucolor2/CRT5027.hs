{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
{-# LANGUAGE ViewPatterns, LambdaCase #-}
module Hardware.Compucolor2.CRT5027 where

import Clash.Prelude

import RetroClash.Clock
import RetroClash.Port
import RetroClash.VGA
import RetroClash.Video
import RetroClash.Utils
import RetroClash.Barbies

import Control.Monad.State
import Barbies.TH
import Control.Lens hiding (Index)

type TextWidth = 64
type TextHeight = 32

data S = MkS
    { _cursorX :: Unsigned 8
    , _cursorY :: Unsigned 8
    , _lastLine :: Index TextHeight
    }
    deriving (Show, Generic, NFDataX)
makeLenses ''S

initS :: S
initS = MkS
    { _cursorX = 0
    , _cursorY = 0
    , _lastLine = 0
    }

declareBareB [d|
  data Output = MkOutput
      { cursor :: Maybe (Unsigned 8, Unsigned 8)
      , scrollOffset :: Index TextHeight
      } |]

crt5027
    :: (HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Maybe (PortCommand (Index 16) (Unsigned 8)))
    -> ( Signal dom (Maybe (Unsigned 8))
       , (Signals dom Output, Signal dom Bool)
       )
crt5027 frameEnd cmd = (dataOut, (crtOut, blink))
  where
    blink = riseEveryWhen (SNat @32) frameEnd
    blinkState = oscillateWhen True blink

    (dataOut, bunbundle -> crtOut) = mealyStateB step initS (cmd, blink, blinkState)

    step (cmd, blink, blinkState) = do
        dataOut <- traverse exec cmd
        scrollOffset <- nextIdx <$> use lastLine
        x <- use cursorX
        y <- use cursorY
        let cursor = (x, y) <$ guard blinkState
        return (dataOut, MkOutput{..})

exec :: PortCommand (Index 16) (Unsigned 8) -> State S (Unsigned 8)
exec cmd = case cmd of
    ReadPort 0x8 -> use cursorX
    ReadPort 0x9 -> use cursorY
    WritePort port val -> (*> return 0x00) $ case port of
        0x6 -> lastLine .= bitCoerce (resize val)
        0xb -> lastLine %= nextIdx
        0xc -> cursorX .= val
        0xd -> cursorY .= val
        _ -> return ()
