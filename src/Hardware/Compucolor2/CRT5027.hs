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
import Data.Foldable (for_)
import Barbies.TH
import Control.Lens hiding (Index)

type TextWidth = 64
type TextHeight = 32

data S = MkS
    { _cursorX :: Unsigned 8
    , _cursorY :: Unsigned 8
    }
    deriving (Show, Generic, NFDataX)
makeLenses ''S

initS :: S
initS = MkS
    { _cursorX = 0
    , _cursorY = 0
    }

declareBareB [d|
  data Output = MkOutput
      { cursor :: Maybe (Unsigned 8, Unsigned 8)
      , blink :: Bool
      } |]

crt5027
    :: (HiddenClockResetEnable dom)
    => Signal dom Bool
    -> Signal dom (Maybe (PortCommand (Index 16) (Unsigned 8)))
    -> ( Signal dom (Maybe (Unsigned 8))
       , Signals dom Output
       )
crt5027 frameEnd cmd = (dataOut, crtOut)
  where
    blink = riseEveryWhen (SNat @32) frameEnd
    blinkState = oscillateWhen True blink

    (dataOut, bunbundle -> crtOut) = unbundle . mealyState step initS . bundle $ (cmd, blink, blinkState)

    step (cmd, blink, blinkState) = do
        for_ cmd $ \case
            WritePort 0xb y -> return () -- TODO: scrolling
            WritePort 0xc x -> cursorX .= x
            WritePort 0xd y -> cursorY .= y
            _ -> return ()

        x <- use cursorX
        y <- use cursorY
        let cursor = (x, y) <$ guard blinkState

        return (Just 0x00, MkOutput{..})
