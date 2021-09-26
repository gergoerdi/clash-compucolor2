module Hardware.Compucolor2
    ( topEntity
    ) where

import Clash.Prelude
import Clash.Annotations.TH

topEntity
    :: "CLK"   ::: Clock System
    -> "RESET" ::: Reset System
    -> "VGA"   ::: Signal System (Unsigned 8)
topEntity clk rst = withClockResetEnable clk rst enableGen video
  where
    video :: (HiddenClockResetEnable dom) => Signal dom (Unsigned 8)
    video = (!!) <$> palette <*> 0
      where
        attr = register 0 attr
        palette = mux (pure False) (pure $ repeat 0) $ bundle $
            attr :>
            Nil

makeTopEntity 'topEntity
