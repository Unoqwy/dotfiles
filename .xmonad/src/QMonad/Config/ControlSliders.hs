module QMonad.Config.ControlSliders (
  opacityControlSlider
) where

import XMonad
import QMonad.Config.Env
import QMonad.Lib.Sliders
import qualified XMonad.Util.ExtensibleState as XS
import QMonad.Lib.Xov

controlSlider :: (XovConf -> XovConf) -> X Int -> (Int -> X()) -> X()
controlSlider defConf' get hk = do
    val <- get
    mkSlider cfg (controlSliderHook hk) val
  where cfg = \conf -> do
          defConf' . defConf $ conf

controlSliderHook :: (Int -> X()) -> SliderUpdateHook
controlSliderHook hk i = do
  hk i
  return $ Just i

opacityControlSlider :: X()
opacityControlSlider = controlSlider (\c -> c { icon = Just "\xf186" }) getOpacity opacityHook

getOpacity :: X Int
getOpacity = do
  XS.gets globalOpacity

opacityHook :: Int -> X()
opacityHook val = do
  envConfig <- XS.gets envConfig
  XS.put $ EnvConfig' {
      envConfig = envConfig,
      globalOpacity = val
    }

defConf :: XovConf -> XovConf
defConf conf = conf {
    showValue = True,
    valueSuffix = Just "%"
  }
