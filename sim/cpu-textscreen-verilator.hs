{-# LANGUAGE RecordWildCards #-}
import Clash.Prelude hiding ((!), lift)

import System.Terminal

import Clash.Clashilator.FFI
import Foreign.Storable
import Foreign.Marshal.Alloc

import Hardware.Compucolor2.Sim
import Hardware.Compucolor2.Sim.Terminal
import Hardware.Compucolor2.Video

import Data.Array.IO
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Data.Foldable (traverse_)
import Data.Traversable (for)

{-# INLINE withRunner #-}
withRunner :: ((INPUT -> IO OUTPUT) -> IO a) -> IO a
withRunner act = alloca $ \inp -> alloca $ \outp -> do
    sim <- simInit
    let step input = do
            poke inp input
            simStep sim inp outp
            peek outp
    x <- act step
    simShutdown sim
    return x

main :: IO ()
main = withRunner $ \runCycle -> do
    vidRAM <- newArray (minBound, maxBound) 0x20

    flip evalStateT Nothing $ withTerminal $ runTerminalT $ do
        eraseInDisplay EraseAll
        hideCursor
        setAutoWrap False

        forever $ do
            replicateM_ 20000 $ do
                vidRead <- lift get
                let input = INPUT
                        { iVID_READ = toFFI vidRead
                        }
                OUTPUT{..} <- liftIO $ runCycle input
                    { iVID_READ = toFFI vidRead
                    }
                let vidAddr = fromFFI oVID_ADDR :: Maybe (Bool, VidAddr)
                    vidWrite = fromFFI oVID_WRITE :: Maybe (Unsigned 8)
                vidRead <- liftIO $ for vidAddr $ \(prio, addr) -> do
                    x <- readArray vidRAM addr
                    traverse_ (writeArray vidRAM addr) vidWrite
                    return x
                lift $ put vidRead

            putScreen vidRAM
