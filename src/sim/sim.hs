{-# LANGUAGE OverloadedStrings, NumericUnderscores #-}
import Clash.Prelude hiding ((!))

import RetroClash.Sim.IO
import RetroClash.Sim.SDL
import RetroClash.Barbies
import Hardware.Compucolor2

import Data.Array.IO
import Data.Array ((!))
import Control.Monad
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Control.Monad.IO.Class
import Data.Word
import Data.Tuple.Curry

main :: IO ()
main = do
    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        return $ rasterizePattern @800 @600 $ \x y -> (fromIntegral x, fromIntegral y, 0)

videoParams :: VideoParams
videoParams = MkVideoParams
    { windowTitle = "Compucolor 2"
    , screenScale = 2
    , screenRefreshRate = 60
    , reportFPS = True
    }
