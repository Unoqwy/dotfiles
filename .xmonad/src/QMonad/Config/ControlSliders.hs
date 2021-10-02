module QMonad.Config.ControlSliders (
  opacityControlSlider,
  brightnessControlSlider,
  volumeControlSlider,
  colorTempControlSlider,
) where

import XMonad
import XMonad.Util.Run (runProcessWithInput)
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import QMonad.Config.Env
import QMonad.Config.Hooks.Manage (applyOpacityRule)
import QMonad.Lib.Sliders
import QMonad.Lib.Xov

import Codec.Binary.UTF8.String (encodeString)
import System.Process (runInteractiveCommand, waitForProcess)
import System.IO
import System.Exit (ExitCode (ExitSuccess))

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
brightnessControlSlider = do
  e <- runSHGetExitCode "brightnessctl --class 'brightness' max"
  ctrl <- case e of
      ExitSuccess -> return . Just $ (getBrightnessCtlVal, brightnessCtlHook, return 100)
      _ -> do
        e' <- runSHGetExitCode "ddcutil -t detect | grep 'Monitor:'"
        sid <- gets $ W.screen . W.current . windowset
        let d = 2 - fromIntegral sid
        return $ if e' == ExitSuccess
          then Just (ddcutilQuery d Val, ddcutilHook d, ddcutilQuery d Max)
          else Nothing
  case ctrl of
    Just (get, set, gm) -> do
      max <- gm
      controlSlider (\c -> c {
        icon = constIcon "\xf042",
        maxValue = max
      }) id (Just $ extendKeybindings (defaultKeybindings 1 5) (M.fromList [
          ((0, xF86XK_MonBrightnessUp  ), incVal 5)
        , ((0, xF86XK_MonBrightnessDown), incVal (-5))
        ])) get set
    _ -> return ()

type DisplayId = Int
data BrightnessQuery = Val | Max deriving (Enum)

ddcutil :: String
ddcutil = "ddcutil --sleep-less --sleep-multiplier .125"

ddcutilQuery :: DisplayId -> BrightnessQuery -> X Int
ddcutilQuery d Val = read <$> runSH (ddcutil ++ " -t -d " ++ show d ++ " getvcp 10 | cut -d 'C' -f 3 | awk '{ print $1; }'")
ddcutilQuery d Max = read <$> runSH (ddcutil ++ " -t -d " ++ show d ++ " getvcp 10 | cut -d 'C' -f 3 | awk '{ print $2; }'")

ddcutilHook :: DisplayId -> Int -> X()
ddcutilHook d val = awaitSpawn $ ddcutil ++ " -d " ++ show d ++ " setvcp 10 " ++ show val

getBrightnessCtlVal :: X Int
getBrightnessCtlVal = do
  conf <- XS.gets envConfig
  brightness <- runProcessWithInput (localBin conf "get-brightness") [] []
  return $ read brightness

brightnessCtlHook :: Int -> X()
brightnessCtlHook val = awaitSpawn $ "brightnessctl set " ++ show val ++ "%"

volumeControlSlider :: X()
volumeControlSlider = controlSlider (\c -> c {
    icon = Just $ \val -> do
      muted <- runSH "pamixer --get-mute"
      return $ if muted == "false" then case val of
        x | x <= 30 -> "\xf027"
        x | x >= 60 -> "\xf028"
        _ -> "\xf6a8"
      else "\xf6a9"
  }) id (Just $ extendKeybindings (defaultKeybindings 1 5) (M.fromList [
      ((0, xK_space), sDo . awaitSpawn $ "pamixer --toggle-mute")
    , ((0, xF86XK_AudioMute), sDo . awaitSpawn $ "pamixer --toggle-mute")
    , ((0, xF86XK_AudioRaiseVolume), incVal 5)
    , ((0, xF86XK_AudioLowerVolume), incVal (-5))
  ])) getVolume volumeHook

getVolume :: X Int
getVolume = read <$> io (runSH "pamixer --get-volume")

volumeHook :: Int -> X()
volumeHook val = awaitSpawn $ "pamixer --set-volume " ++ show val

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

awaitSpawn :: String -> X()
awaitSpawn = void . runSH

runSHGetExitCode :: MonadIO m => String -> m ExitCode
runSHGetExitCode cmd = io $ do
  uninstallSignalHandlers
  (hIn, hOut, hErr, p) <- runInteractiveCommand cmd
  mapM_ hClose [hIn, hErr]
  waitForProcess p <* hGetContents hOut <* installSignalHandlers

runSH :: MonadIO m => String -> m String
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
