module Hardware.Compucolor2.Sim.Clash where

import Clash.Prelude
import RetroClash.Sim.IO
import Hardware.Compucolor2
import Data.Array.IO
import Data.Foldable (traverse_)
import Data.Traversable (for)

mkSim :: IO (IOArray VidAddr (Unsigned 8), IO ())
mkSim = do
    vidRAM <- newArray (minBound, maxBound) 0x00
    sim <- simulateIO_ @System (bundle . simBoard) Nothing
    let step = do
            sim $ \(vidAddr, vidWrite) -> for vidAddr $ \(prio, addr) -> do
                x <- readArray vidRAM addr
                traverse_ (writeArray vidRAM addr) vidWrite
                return x
    return (vidRAM, step)
