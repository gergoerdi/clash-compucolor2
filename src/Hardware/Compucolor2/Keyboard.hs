module Hardware.Compucolor2.Keyboard (keyboard) where

import Clash.Prelude
import RetroClash.Utils
import RetroClash.PS2
import RetroClash.Keypad

keyboard
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe ScanCode)
    -> Signal dom (BitVector 8)
    -> Signal dom (BitVector 8)
keyboard sc selector = register 0 $ complement <$> mux includeMods (withMods <$> mods <*> cols) cols
  where
    keys = bundle $ keyboardState sc
    row = resize @BitVector @8 @4 <$> selector
    cols = keys .!!. row

    includeMods = not . (`testBit` 7) <$> selector

    mods = fmap pack . bundle . reverse $
        ctrl :> shift :> rpt :> capsLock :> Nil
      where
        key = (keyPress =<<) <$> sc

        ctrl = keyState 0x014 sc .||. keyState 0x114 sc
        shift = keyState 0x012 sc .||. keyState 0x059 sc
        rpt = keyState 0x011 sc .||. keyState 0x111 sc -- left/right Alt
        capsLock = oscillateWhen True $ key .== Just 0x058

    withMods :: BitVector 4 -> BitVector 8 -> BitVector 8
    withMods mods cols = pack (mods, resize cols)

keyboardState
    :: forall dom. (HiddenClockResetEnable dom)
    => Signal dom (Maybe ScanCode)
    -> Vec 16 (Signal dom (BitVector 8))
keyboardState sc = map keyRow $ transpose keymap
  where
    keyRow :: Vec 8 KeyCode -> Signal dom (BitVector 8)
    keyRow = fmap (pack . reverse) . bundle . map (\kc -> keyState kc sc)

keymap :: Matrix 8 16 KeyCode
keymap =
    (0x045 :> 0x016 :> 0x01e :> 0x026 :> 0x025 :> 0x02e :> 0x036 :> 0x03d :>
     0x03e :> 0x046 :> 0x04c :> 0x052 :> 0x041 :> 0x04e :> 0x049 :> 0x14a :>
     Nil) :>
    (0x000 :> 0x01c :> 0x032 :> 0x021 :> 0x023 :> 0x024 :> 0x02b :> 0x034 :>
     0x033 :> 0x043 :> 0x03b :> 0x042 :> 0x04b :> 0x03a :> 0x031 :> 0x044 :>
     Nil) :>
    (0x04d :> 0x015 :> 0x02d :> 0x01b :> 0x02c :> 0x03c :> 0x02a :> 0x01d :>
     0x022 :> 0x035 :> 0x01a :> 0x054 :> 0x05b :> 0x058 :> 0x05d :> 0x04e :>
     Nil) :>
    repeat 0 :>
    (0x066 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :>
     0x16c :> 0x00d :> 0x172 :> 0x000 :> 0x000 :> 0x05a :> 0x000 :> 0x000 :>
     Nil) :>
    (0x000 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :>
     0x000 :> 0x174 :> 0x16b :> 0x076 :> 0x175 :> 0x000 :> 0x000 :> 0x000 :>
     Nil) :>
    (0x029 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :>
     0x000 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :> 0x000 :>
     Nil) :>
    repeat 0 :>
    Nil
