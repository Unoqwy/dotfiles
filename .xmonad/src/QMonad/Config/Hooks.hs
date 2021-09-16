module QMonad.Config.Hooks (
    QMonad.Config.Hooks.startupHook,
    QMonad.Config.Hooks.layoutHook,
    QMonad.Config.Hooks.logHook,
    QMonad.Config.Hooks.scratchpads,
    QMonad.Config.Hooks.manageHook
) where

import XMonad hiding (Hide)
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat)
import XMonad.Util.SpawnOnce (spawnOnce)
import qualified XMonad.StackSet as W
import Data.Bifunctor (first)

import XMonad.Layout.BoringWindows
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.Minimize
import XMonad.Layout.Named (named)
import XMonad.Util.NamedScratchpad (NamedScratchpad(..), customFloating, namedScratchpadManageHook)
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.ThreeColumns
import QMonad.Lib.WorkspaceMasks (setWorkspaceMask)

import System.Environment (lookupEnv, setEnv)

import qualified QMonad.Config.Applications as A
-- Startup hook
setupDefaultWorkspaces :: X()
setupDefaultWorkspaces = do
    mapM_ (\w -> setWorkspaceMask w Nothing (Just False)) hidden
    mapM_ (\t -> setWorkspaceMask (fst t) (Just $ snd t) Nothing) named
  where hidden = map show ([0, 1, 5] ++ [7..9]) ++ ["NSP"]
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
  -- start daemons
  spawnOnce "flameshot &"
  spawnOnce "greenclip daemon &"
  spawnOnce "dunst &"
  spawnOnce "unread-bell &"

  -- compositor
  spawnOnce "picom --config $XDG_CONFIG_HOME/picom/picom.conf &"

  xmonad_started <- liftIO $ lookupEnv "XMONAD_STARTED"
  case xmonad_started of
    Just "1" -> spawn "notify-send 'xmonad' 'Restart OK'"
    _ -> liftIO $ setEnv "XMONAD_STARTED" "1"

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

layouts = named "V 2*|1" vtall
      ||| named "H 1|1" htall
      ||| named "H 2|3/2" three
      ||| named "Full" full
      ||| named "" fullScreen
  where
    defaultIncr = 5/100
    htall = avoidStruts . mkGaps $ Tall 1 defaultIncr (1/2)
    vtall = avoidStruts . mkGaps $ Mirror $ Tall 2 defaultIncr (2/3)
    three = avoidStruts . mkGaps $ ThreeCol 1 defaultIncr (2/5)
    full  = avoidStruts . mkGaps . noBorders $ Full
    fullScreen = noBorders Full

layoutHook = minimize . boringWindows . smartBorders $ layouts

-- Log hook
logHook :: X()
logHook = do
    return ()

-- Named scratchpads
scratchpads = [
      NS "floaterm" (A.spawnTermWithClass "floaterm" Nothing) (resource =? "floaterm") float
    , NS "floaterm-min" (A.spawnTermWithClass "floaterm-min" Nothing) (resource =? "floaterm-min") floatMin
    , NS "quicksearch" "vimb --name quicksearch" (resource =? "quicksearch") float
    , NS "filexplorer" (A.spawnTermWithClass "floatfe" (Just "xplr")) (resource =? "floatfe") float
  ] where
      float = customFloating $ W.RationalRect 0.1 0.1 0.8 0.8
      floatMin = customFloating $ W.RationalRect 0.15 0.15 0.7 0.7

-- Manage hook
manageHook :: ManageHook
manageHook = namedScratchpadManageHook scratchpads <+> composeAll [
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
  , resource =? "sxiv" --> doCenterFloat

  -- Firefox PiP
  , wmName =? "Picture-in-Picture" --> doFloat

  -- Misc
  , resource =? "Godot_Engine" --> doFloat
  , wmRole =? "GtkFileChooserDialog" --> doCenterFloat
  ] where
      wmName = stringProperty "WM_NAME"
      wmRole = stringProperty "WM_WINDOW_ROLE"
