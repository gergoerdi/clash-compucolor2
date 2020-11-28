{-# LANGUAGE RecordWildCards #-}
import Clash.Shake
import Clash.Shake.Xilinx

import Development.Shake
import Development.Shake.FilePath
import Control.Monad

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

    outDir </> "*.bin" %> \out -> do
        let inp = "image" </> dropExtension (takeFileName out)
        binImage Nothing inp out

    kit@ClashKit{..} <- clashRules (outDir </> "clash") Verilog
        [ "src" ]
        "Hardware.Compucolor2"
        [ "-Wno-partial-type-signatures"
        -- , "-fclash-inline-limit=600"
        ] $
        need [outDir </> rom <.> "bin" | rom <- roms]

    forM_ targets $ \(name, synth) -> do
        SynthKit{..} <- synth kit (outDir </> name </> "synth") ("target" </> name) "Compucolor2"

        mapM_ (uncurry $ nestedPhony name) $
          ("bitfile", need [bitfile]):
          phonies

    phony "clashi" $
      clash ["--interactive", "src/Hardware/Compucolor2.hs"]
