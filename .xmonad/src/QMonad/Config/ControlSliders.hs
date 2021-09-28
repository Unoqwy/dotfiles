module QMonad.Config.ControlSliders (
  opacityControlSlider,
  brightnessControlSlider,
) where

import XMonad
import XMonad.Util.Run (runProcessWithInput)
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W

import QMonad.Config.Env
import QMonad.Config.Hooks.Manage (applyOpacityRule)
import QMonad.Lib.Sliders
import QMonad.Lib.Xov

opacityControlSlider :: X()
opacityControlSlider = controlSlider (\c -> c { icon = Just "\xf8fb" }) getOpacity opacityHook

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
  withWindowSet $ \ws -> do
    let wss = map (W.integrate' . W.stack) (W.hidden ws ++ map W.workspace (W.current ws : W.visible ws))
        windows = foldl1 (++) wss
    mapM_ applyOpacityRule windows

brightnessControlSlider :: X()
brightnessControlSlider = controlSlider (\c -> c { icon = Just "\xf042" }) getBrightness brightnessHook

getBrightness :: X Int
getBrightness = do
  conf <- XS.gets envConfig
  brightness <- runProcessWithInput (localBin conf "get-brightness") [] []
  return $ read brightness

brightnessHook :: Int -> X()
brightnessHook val = do
  spawn $ "brightnessctl set " ++ show val ++ "%"

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


defConf :: XovConf -> XovConf
defConf conf = conf {
    showValue = True,
    valueSuffix = Just "%"
  }
