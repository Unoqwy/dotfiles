module QMonad.Lib.Sliders (
  SliderUpdateHook(..),
  SliderKeybindings(..),
  mkSlider,
  defaultKeybindings,
  extendKeybindings,

  sId,
  sDo,
  sQuit,
  incVal,
) where

import Prelude hiding (Left, Right)
import XMonad hiding (borderWidth)
import Control.Monad (void, when)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)
import GHC.Conc (forkIO)
import QMonad.Lib.Xov
import qualified XMonad.StackSet as W
import qualified Data.Map as M

type SliderUpdateHook = Int -> X (Maybe Int)
type SliderKeybindings = M.Map (KeyMask, KeySym) SliderUpdateHook

mkSlider :: (XovConf -> XovConf) -> (XovStyle -> XovStyle) -> SliderKeybindings -> SliderUpdateHook -> Int -> X()
mkSlider defConf style kbs hk val = do
  sid <- gets $ W.screen . W.current . windowset
  withDisplay $ \dpy -> void $ initSlider dpy sid defConf style kbs hk val

initSlider :: Display -> ScreenId -> (XovConf -> XovConf) -> (XovStyle -> XovStyle) -> SliderKeybindings -> SliderUpdateHook -> Int -> X Int
initSlider dpy sid defConf defStyle kbs hk val = do
  let conf = defConf XovConf {
        icon = Nothing,
        iconWidth = 40,
        showValue = False,
        valueWidth = 60,
        valueSuffix = Nothing,
        width = 400,
        height = 40,
        borderWidth = 2,
        innerBorderWidth = 2,
        padding = 3,
        minValue = 0,
        maxValue = 100
      }
  let style = defStyle XovStyle {
        iconFont = "Font Awesome 5 Pro:size=16:style=Solid",
        iconColor = "#a3a1a4",
        valueFont = "Jetbrains Mono:size=14",
        valueColor = "#a3a1a4",
        borderColor = "#4c516d",
        innerBorderColor = "#4c516d",
        progressColor = "#6f759b",
        emptyColor = "#242530",
        backgroundColor = "#242530"
      }
  overlay <- io $ mkOverlay dpy sid HCenter VCenter conf style val

  let w = win overlay
  io $ selectInput dpy w keyPressMask
  status <- liftIO $ grabKeyboard dpy w True grabModeAsync grabModeAsync currentTime
  val' <- promptValue dpy overlay kbs hk val

  io $ destroyOverlay dpy overlay
  return val'

promptValue :: Display -> XovOverlay -> SliderKeybindings -> SliderUpdateHook -> Int -> X Int
promptValue dpy o kbs hk val = do
  call <- io $ eventHandler dpy kbs
  val' <- call val
  case val' of
     Just v -> do
       val'' <- hk $ inBounds o v
       case val'' of
            Just v' -> do
              let newv = inBounds o v'
              io $ updateProgress dpy o newv
              promptValue dpy o kbs hk newv
            Nothing -> return val
     Nothing -> return val

inBounds :: XovOverlay -> Int -> Int
inBounds o v = min (max v (minValue . conf $ o)) (maxValue . conf $ o)

eventHandler :: Display -> M.Map (KeyMask, KeySym) SliderUpdateHook -> IO SliderUpdateHook
eventHandler dpy keymap = allocaXEvent $ \e -> do
  nextEvent dpy e
  ev <- getEvent e
  if ev_event_type ev == keyPress then do
    (ks,s) <- lookupString $ asKeyEvent e
    return $ \i -> do
      mask <- cleanMask (ev_state ev)
      let keybinding = M.lookup (mask, fromMaybe xK_VoidSymbol ks) keymap
      fromMaybe sId keybinding i
  else eventHandler dpy keymap

sId :: SliderUpdateHook
sId = return . Just

sDo :: X() -> SliderUpdateHook
sDo action i = do
  action
  return $ Just i

sQuit :: SliderUpdateHook
sQuit _ = return Nothing

incVal :: Int -> SliderUpdateHook
incVal inc val = do
  return $ Just (val + inc)

defaultKeybindings :: Int -> Int -> SliderKeybindings
defaultKeybindings sg bg = M.fromList
  [ ((0, xK_Escape), sQuit)
  , ((0, xK_q), sQuit)
  , ((0, xK_Left), incVal (-bg))
  , ((0, xK_Right), incVal bg)
  , ((0, xK_Up), incVal sg)
  , ((0, xK_Down), incVal (-sg))
  , ((0, xK_h), incVal (-bg))
  , ((0, xK_l), incVal bg)
  , ((0, xK_k), incVal sg)
  , ((0, xK_j), incVal (-sg))
  ]

extendKeybindings :: SliderKeybindings -> SliderKeybindings -> SliderKeybindings
extendKeybindings orig extend = M.union extend orig
