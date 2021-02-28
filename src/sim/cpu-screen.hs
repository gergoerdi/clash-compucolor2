import Clash.Prelude hiding ((!), lift)

import RetroClash.Sim.SDL

import Hardware.Compucolor2.Sim.Clash
import Hardware.Compucolor2.Sim.SDL

import Data.Array.IO
import qualified Data.ByteString as BS
import Control.Monad
import Control.Monad.Trans

main :: IO ()
main = do
    fontPath <- return "image/chargen.uf7"
    fontBS <- BS.readFile fontPath
    fontROM <- newArray @IOArray (minBound, maxBound) 0
    zipWithM_ (writeArray fontROM) [0..] (fmap bitCoerce $ BS.unpack fontBS)
    fontROM <- freeze fontROM

    (vidRAM, step) <- mkSim

    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        liftIO $ replicateM_ 20000 step
        lift $ renderScreen fontROM vidRAM
