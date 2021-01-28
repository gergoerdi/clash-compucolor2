{-# LANGUAGE RecordWildCards #-}
import Clash.Prelude hiding ((!), lift)

import RetroClash.Sim.SDL

import Clash.Clashilator.FFI
import Foreign.Storable
import Foreign.Marshal.Alloc

import Hardware.Compucolor2.Sim
import Hardware.Compucolor2.Sim.SDL
import Hardware.Compucolor2.Video

import Data.Array.IO
import qualified Data.ByteString as BS
import Control.Monad
import Control.Monad.Trans
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Control.Monad.State

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
    fontPath <- return "image/chargen.uf7"
    fontBS <- BS.readFile fontPath
    fontROM <- newArray @IOArray (minBound, maxBound) 0
    zipWithM_ (writeArray fontROM) [0..] (fmap bitCoerce $ BS.unpack fontBS)
    fontROM <- freeze fontROM

    vidRAM <- newArray @IOArray (minBound, maxBound) 0x20

    flip evalStateT Nothing $ withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        replicateM_ 20000 $ do
            vidRead <- get
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
            put vidRead

        liftIO $ renderScreen fontROM vidRAM
