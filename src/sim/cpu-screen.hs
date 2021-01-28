import Clash.Prelude hiding ((!), lift)

import RetroClash.Sim.SDL
import RetroClash.Sim.IO

import Hardware.Compucolor2
import Hardware.Compucolor2.Sim.SDL

import Data.Array.IO
import qualified Data.ByteString as BS
import Control.Monad
import Control.Monad.Trans
import Data.Foldable (traverse_)
import Data.Traversable (for)

main :: IO ()
main = do
    fontPath <- return "image/chargen.uf7"
    fontBS <- BS.readFile fontPath
    fontROM <- newArray @IOArray (minBound, maxBound) 0
    zipWithM_ (writeArray fontROM) [0..] (fmap bitCoerce $ BS.unpack fontBS)
    fontROM <- freeze fontROM

    vidRAM <- newArray (minBound, maxBound) 0x20

    sim <- simulateIO_ @System (bundle . simBoard) Nothing

    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        replicateM_ 20000 $ liftIO $ do
            sim $ \(vidAddr, vidWrite) -> for vidAddr $ \(prio, addr) -> do
                x <- readArray vidRAM addr
                traverse_ (writeArray vidRAM addr) vidWrite
                return x

        lift $ renderScreen fontROM vidRAM
