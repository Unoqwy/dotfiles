module QMonad.Config.ControlSliders (
  mkSlider
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

mkSlider :: Int -> X()
mkSlider val = do
  sid <- gets $ W.screen . W.current . windowset
  withDisplay $ \dpy -> void $ initSlider dpy sid val

initSlider :: Display -> ScreenId -> Int -> X Int
initSlider dpy sid val = do
  let conf = XovConf {
        icon = Just "\xf58f",
        iconWidth = 40,
        showValue = True,
        valueWidth = 60,
        valuePrefix = Just "%",
        width = 400,
        height = 40,
        borderWidth = 2,
        innerBorderWidth = 2,
        padding = 3,
        maxValue = 100
      }
  let style = XovStyle {
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
  val' <- promptValue dpy overlay val

  io $ destroyOverlay dpy overlay
  return val'

promptValue :: Display -> XovOverlay -> Int -> X Int
promptValue dpy o val = do
  call <- io $ eventHandler dpy keyBindings
  val' <- call val
  case val' of
     Just v -> do
       let newv = min (max v 0) (maxValue . conf $ o)
       io $ drawOverlay dpy o newv
       io $ sync dpy True
       promptValue dpy o newv
     Nothing -> return val

eventHandler :: Display -> M.Map (KeyMask, KeySym) (Int -> X (Maybe Int)) -> IO (Int -> X (Maybe Int))
eventHandler dpy keymap = allocaXEvent $ \e -> do
  nextEvent dpy e
  ev <- getEvent e
  if ev_event_type ev == keyPress then do
    (ks,s) <- lookupString $ asKeyEvent e
    return $ \i -> do
      mask <- cleanMask (ev_state ev)
      let keybinding = M.lookup (mask, fromMaybe xK_VoidSymbol ks) keymap
      fromMaybe (return . Just) keybinding i
  else eventHandler dpy keymap

incVal :: Int -> Int -> X (Maybe Int)
incVal inc val = do
  spawn "notify-send 'test'"
  return $ Just (val + inc)

keyBindings :: M.Map (KeyMask, KeySym) (Int -> X (Maybe Int))
keyBindings = M.fromList
  [ ((0, xK_Escape), \i -> return Nothing)
  , ((0, xK_Left), incVal (-5))
  , ((0, xK_Right), incVal 5)
  ]
