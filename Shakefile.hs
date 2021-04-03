{-# LANGUAGE RecordWildCards, ViewPatterns, DataKinds #-}
{-# LANGUAGE NoStarIsType, TypeOperators, TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
import Clash.Prelude hiding (filter)

import Clash.Shake
import Clash.Shake.Xilinx

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Config
import Control.Monad
import Text.Regex.Applicative
import Text.Regex.Applicative.Common
import Data.Filtrable
import Data.Maybe (fromMaybe)
import Data.Traversable (for)

targets =
    [ ("nexys-a7-50t", xilinxVivado nexysA750T)
    -- , ("papilio-pro",  xilinxISE papilioPro)
    -- , ("papilio-one",  xilinxISE papilioOne)
    ]

outDir = "_build"

main :: IO ()
main = shakeArgs shakeOptions{ shakeFiles = outDir } $ do
    useConfig "build.mk"

    phony "clean" $ do
        putNormal $ "Cleaning files in " <> outDir
        removeFilesAfter outDir [ "//*" ]

    let roms = ["chargen.uf6", "v678.rom"]

    outDir </> "disk.tracks" %> \out -> do
        ccvf <- fromMaybe "image/disks/sampler.ccvf" <$> getConfig "DISK"
        tracks <- parseDisk <$> readFile' ccvf
        writeFileChanged out $ unlines $ diskLines tracks

    outDir </> "*.bin" %> \out -> do
        let inp = "image" </> dropExtension (takeFileName out)
        binImage Nothing inp out

    kit@ClashKit{..} <- clashRules (outDir </> "clash") Verilog
        [ "src" ]
        "Hardware.Compucolor2"
        [ "-Wno-partial-type-signatures"
        -- , "-fclash-inline-limit=600"
        ] $ do
        need [outDir </> rom <.> "bin" | rom <- roms]
        need [outDir </> "disk.tracks"]

    forM_ targets $ \(name, synth) -> do
        SynthKit{..} <- synth kit (outDir </> name </> "synth") ("target" </> name) "Compucolor2"

        mapM_ (uncurry $ nestedPhony name) $
          ("bitfile", need [bitfile]):
          phonies

    phony "clashi" $
      clash ["--interactive", "src/Hardware/Compucolor2.hs"]

type TrackSize = 15360
type TrackCount = 41

type Track = Vec TrackSize Bit
type Disk = Vec TrackCount Track

diskLines :: Disk -> [String]
diskLines = toList . fmap show . concat

parseDisk :: String -> Disk
parseDisk = fromMaybe (error "CCVF parsing failed") . match disk
  where
    eol = void $ optional (sym '\r') *> sym '\n'

    disk = magic *> many label *> tracks

    magic = string "Compucolor Virtual Floppy Disk Image" *> eol
    label = string "Label " *> few anySym <* eol

    tracks = for indicesI $ \i -> trackHeader i *> track

    trackHeader i = string "Track " *> filter (== i) decimal *> eol

    track = bits <$> (sequenceA . repeat $ byte <* many eol)

    byte = ((++#) @4 @4) <$> hexDigit <*> hexDigit

    bits :: Vec n (BitVector 8) -> Vec (n * 8) Bit
    bits = concatMap (reverse . bv2v)
