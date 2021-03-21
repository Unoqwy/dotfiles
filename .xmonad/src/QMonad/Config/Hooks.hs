module QMonad.Config.Hooks (
    QMonad.Config.Hooks.startupHook,
    QMonad.Config.Hooks.layouts,
    QMonad.Config.Hooks.logHook,
    QMonad.Config.Hooks.manageHook
) where

import XMonad hiding (Hide)
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat)
import XMonad.Util.SpawnOnce (spawnOnce)
import qualified XMonad.StackSet as W
import Data.Bifunctor (first)

import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.ThreeColumns
import QMonad.Lib.WorkspaceMasks (setWorkspaceMask)

-- Startup hook
setupDefaultWorkspaces :: X()
setupDefaultWorkspaces = do
    mapM_ (\w -> setWorkspaceMask w Nothing (Just False)) hidden
    mapM_ (\t -> setWorkspaceMask (fst t) (Just $ snd t) Nothing) named
  where hidden = map show ([0, 1, 5] ++ [7..9])
        named  = map (Data.Bifunctor.first show) [
              (0, "config")
            , (1, "music")
            , (2, "web")
            , (3, "dev")
            , (4, "main")
            , (5, "any")
            , (6, "chat")
            , (7, "ts")
            , (8, "tm")
            ]

startupHook :: X()
startupHook = do
  spawnOnce "flameshot &" -- Flameshot needs to be running in order to open the GUI
  spawnOnce "picom --config $XDG_CONFIG_HOME/picom/picom.conf &"

  setupDefaultWorkspaces

-- Layouts
windowSpacing = 4 `div` 2
screenSpacing = [ 6 - windowSpacing -- Top
                , 6 - windowSpacing -- Bottom
                , 6 - windowSpacing -- Right
                , 6 - windowSpacing -- Left
                ]

mkGaps = spacingRaw False
  (Border (head screenSpacing) (screenSpacing!!1) (screenSpacing!!2) (screenSpacing!!3)) True
  (Border windowSpacing windowSpacing windowSpacing windowSpacing) True

defaultIncr = 5/100
layouts = named "V 2*|1" vtall
      ||| named "H 1|1" htall
      ||| named "H 2|3/2" three
      ||| named "Full" full
      ||| named "" fullScreen
  where
    htall = avoidStruts . mkGaps $ Tall 1 defaultIncr (1/2)
    vtall = avoidStruts . mkGaps $ Mirror $ Tall 2 defaultIncr (2/3)
    three = avoidStruts . mkGaps $ ThreeCol 1 defaultIncr (2/5)
    full  = avoidStruts . mkGaps . noBorders $ Full
    fullScreen = noBorders Full

-- Log hook
logHook :: X()
logHook = do
    return ()

-- Manage hook
manageHook :: ManageHook
manageHook = composeAll [
    isDialog --> doF W.swapUp

  -- Organized applications
  , className =? "discord" --> doShift "6"
  , className =? "TeamSpeak 3" --> doShift "7"
  , className =? "Notion" --> doShift "8"

  -- Splash screens
  , className =? "jetbrains-idea-ce" <&&> title =? "win0" --> doCenterFloat

  -- Settings apps
  , className =? "Pavucontrol" --> doCenterFloat
  , className =? "flameshot" <&&> title =? "Configuration" --> doCenterFloat

  -- Firefox PiP
  , wmRole =? "PictureInPicture" --> doFloat

  -- Misc
  , resource =? "Godot_Engine" --> doFloat
  ] where
      wmRole = stringProperty "WM_WINDOW_ROLE"
