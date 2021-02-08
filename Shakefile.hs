{-# LANGUAGE RecordWildCards, ViewPatterns, DataKinds #-}
import Clash.Shake
import Clash.Shake.Xilinx

import Development.Shake
import Development.Shake.FilePath
import Control.Monad
import Data.List
import Data.List.Split
import Clash.Prelude (Bit, BitVector, Unsigned, pack, (!))
import Numeric (readHex)

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

    outDir </> "disk/*.track" %> \out -> do
        let i = read $ takeBaseName out

        let inp = "image/disks/hangman.ccvf" -- TODO
        tracks <- map trackBits . splitTracks <$> readFile' inp
        writeFileChanged out $ unlines . map show $ tracks !! i

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
        need [outDir </> "disk/01.track"] -- TODO

    forM_ targets $ \(name, synth) -> do
        SynthKit{..} <- synth kit (outDir </> name </> "synth") ("target" </> name) "Compucolor2"

        mapM_ (uncurry $ nestedPhony name) $
          ("bitfile", need [bitfile]):
          phonies

    phony "clashi" $
      clash ["--interactive", "src/Hardware/Compucolor2.hs"]

splitTracks :: String -> [String]
splitTracks =
    tracks .
    split (dropInitBlank $ whenElt ("Track" `isPrefixOf`)) .
    dropWhile (not . ("Track" `isPrefixOf`)) .
    lines
  where
    tracks ([t]:bs:xs) = trackHeader t `seq` mconcat bs : tracks xs
    tracks [] = []

    trackHeader :: String -> Int
    trackHeader s = maybe (error $ unwords ["Invalid track header:", show s]) read $ stripPrefix "Track " s

trackBits :: String -> [Bit]
trackBits = concatBits . map (pack . byte) . chunksOf 2
  where
    byte :: String -> Unsigned 8
    byte s = case readHex s of [(x, "")] -> x

    concatBits :: [BitVector 8] -> [Bit]
    concatBits = concatMap $ \bv -> [bv!0, bv!1, bv!2, bv!3, bv!4, bv!5, bv!6, bv!7]
