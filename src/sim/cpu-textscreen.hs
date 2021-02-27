import Clash.Prelude hiding ((!), lift)

import RetroClash.Sim.IO

import Hardware.Compucolor2
import Hardware.Compucolor2.Sim.Terminal
import System.Terminal

import Data.Array.IO
import Control.Monad
import Control.Monad.Trans
import Data.Foldable (traverse_)
import Data.Traversable (for)

main :: IO ()
main = do
    vidRAM <- newArray (minBound, maxBound) 0x20

    sim <- simulateIO_ @System (bundle . simBoard) Nothing

    withTerminal $ runTerminalT $ do
        eraseInDisplay EraseAll
        hideCursor
        setAutoWrap False

        forever $ do
            replicateM_ 20000 $ liftIO $ do
                sim $ \(vidAddr, vidWrite) -> for vidAddr $ \(prio, addr) -> do
                    x <- readArray vidRAM addr
                    traverse_ (writeArray vidRAM addr) vidWrite
                    return x

            putScreen vidRAM
