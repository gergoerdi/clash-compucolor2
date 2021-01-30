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
    , _cnt :: Index 60
    }
    deriving (Show, Generic, NFDataX)
makeLenses ''S

initS :: S
initS = MkS
    { _cursorX = 0
    , _cursorY = 0
    , _cnt = 0
    }

declareBareB [d|
  data Output = MkOutput
      { cursor :: Maybe (Unsigned 8, Unsigned 8)
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
    (dataOut, bunbundle -> crtOut) = unbundle . mealyState step initS . bundle $ (cmd, frameEnd)

    step (cmd, frameEnd) = do
        for_ cmd $ \case
            WritePort 0xb y -> return () -- TODO: scrolling
            WritePort 0xc x -> cursorX .= x
            WritePort 0xd y -> cursorY .= y

        x <- use cursorX
        y <- use cursorY
        blink <- uses cnt (< 30)
        let cursor = (x, y) <$ guard blink
        when frameEnd $ cnt %= nextIdx

        return (Just 0x00, MkOutput{..})
