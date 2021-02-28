import Clash.Prelude hiding ((!), lift)

import Hardware.Compucolor2.Sim.Clash
import Hardware.Compucolor2.Sim.Terminal
import System.Terminal

import Data.Array.IO
import Control.Monad
import Control.Monad.Trans

main :: IO ()
main = do
    (vidRAM, step) <- mkSim

    withTerminal $ runTerminalT $ do
        eraseInDisplay EraseAll
        hideCursor
        setAutoWrap False

        forever $ do
            liftIO $ replicateM_ 20000 step
            putScreen vidRAM
