{-# LANGUAGE NumericUnderscores #-}
import Clash.Prelude hiding ((!), lift)

import RetroClash.Sim.SDL

import Hardware.Compucolor2.Sim.Clash
import Hardware.Compucolor2.Sim.SDL

import Data.Array.IO
import qualified Data.ByteString as BS
import Data.Word
import Control.Monad
import Control.Monad.Trans

import Data.Char

loadArray :: (BitPack a, BitSize a ~ BitSize Word8, Num i, Enum i, Ix i) => IOArray i a -> FilePath -> IO ()
loadArray arr path = do
    bs <- BS.readFile path
    let img = bitCoerce <$> BS.unpack bs
    zipWithM_ (writeArray arr) [0..] img

main :: IO ()
main = do
    fontPath <- return "image/chargen.uf7" -- TODO: Use Cabal to discover file?
    fontROM <- newArray @IOArray (minBound, maxBound) 0
    loadArray fontROM fontPath
    fontROM <- freeze fontROM

    (vidRAM, step) <- mkSim

    withMainWindow videoParams $ \events keyDown -> do
        guard $ not $ keyDown ScancodeEscape

        liftIO $ replicateM_ 20000 step
        lift $ renderScreen fontROM vidRAM
