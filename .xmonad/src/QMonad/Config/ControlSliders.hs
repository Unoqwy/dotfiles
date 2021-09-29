module QMonad.Config.ControlSliders (
  opacityControlSlider,
  brightnessControlSlider,
  volumeControlSlider,
  colorTempControlSlider,
) where

import XMonad
import XMonad.Util.Run (runProcessWithInput)
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Functor (($>))
import qualified Data.Map as M

import QMonad.Config.Env
import QMonad.Config.Hooks.Manage (applyOpacityRule)
import QMonad.Lib.Sliders
import QMonad.Lib.Xov

opacityControlSlider :: X()
opacityControlSlider = controlSlider (\c -> c { icon = constIcon "\xf8fb" }) id Nothing getOpacity opacityHook

getOpacity :: X Int
getOpacity = XS.gets globalOpacity

opacityHook :: Int -> X()
opacityHook val = do
  state <- XS.get
  XS.put $ state {
      globalOpacity = val
    }
  withWindowSet $ \ws -> do
    let wss = map (W.integrate' . W.stack) (W.hidden ws ++ map W.workspace (W.current ws : W.visible ws))
        windows = foldl1 (++) wss
    mapM_ applyOpacityRule windows

brightnessControlSlider :: X()
brightnessControlSlider = controlSlider (\c -> c { icon = constIcon "\xf042" }) id Nothing getBrightness brightnessHook

getBrightness :: X Int
getBrightness = do
  conf <- XS.gets envConfig
  brightness <- runProcessWithInput (localBin conf "get-brightness") [] []
  return $ read brightness

brightnessHook :: Int -> X()
brightnessHook val = spawn $ "brightnessctl set " ++ show val ++ "%"

volumeControlSlider :: X()
volumeControlSlider = controlSlider (\c -> c {
    icon = Just $ \_ -> do
      muted <- runSH "pamixer --get-mute"
      return $ if muted == "false" then "\xf6a8" else "\xf6a9"
  }) id (Just $ extendKeybindings (defaultKeybindings 1 5) (M.fromList [
        ((0, xK_space), sDo . void . io $ runSH "pamixer --toggle-mute")
  ])) getVolume volumeHook

getVolume :: X Int
getVolume = read <$> io (runSH "pamixer --get-volume")

volumeHook :: Int -> X()
volumeHook val = spawn $ "pamixer --set-volume " ++ show val

colorTempControlSlider :: X()
colorTempControlSlider = controlSlider (\c -> c {
    icon = constIcon "\xf6df",
    valueSuffix = Nothing,
    minValue = 3000,
    maxValue = 9500
  }) (\s -> s {
    progressColor = "#FFCCBC"
  }) (Just $ defaultKeybindings 100 500) getColorTemp colorTempHook

getColorTemp :: X Int
getColorTemp = XS.gets colorTemp

colorTempHook :: Int -> X()
colorTempHook val = do
  state <- XS.get
  XS.put $ state {
      colorTemp = val
    }
  spawn $ "redshift -PO " ++ show val

runSH :: String -> IO String
runSH command = do
  out <- runProcessWithInput "/bin/sh" ["-c", command] []
  return $ (reverse . dropWhile isSpace . reverse) out

constIcon :: String -> Maybe (Int -> IO String)
constIcon s = Just $ \_ -> return s

controlSlider :: (XovConf -> XovConf) -> (XovStyle -> XovStyle) -> Maybe SliderKeybindings -> X Int -> (Int -> X()) -> X()
controlSlider defConf' style (Just kbs) get hk = do
    val <- get
    mkSlider cfg style kbs (controlSliderHook hk) val
  where cfg = defConf' . defConf
controlSlider defConf' style Nothing get hk = controlSlider defConf' style (Just $ defaultKeybindings 1 5) get hk

controlSliderHook :: (Int -> X()) -> SliderUpdateHook
controlSliderHook hk i = hk i $> Just i

defConf :: XovConf -> XovConf
defConf conf = conf {
    showValue = True,
    valueSuffix = Just "%"
  }
