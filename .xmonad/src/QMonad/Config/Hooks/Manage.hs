module QMonad.Config.Hooks.Manage (manageHook) where

import XMonad hiding (manageHook)
import QMonad.Config.Scratchpads (scratchpads)
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat)
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)
import qualified XMonad.StackSet as W

import QMonad.Config.Env (EnvConfig(..))

import QMonad.Lib.Window.Opacity (setWindowOpacity)

-- Manage hook
manageHook :: EnvConfig -> ManageHook
manageHook conf = namedScratchpadManageHook (scratchpads conf) <+> opacityHook conf

wmName = stringProperty "WM_NAME"
wmRole = stringProperty "WM_WINDOW_ROLE"

windowRules :: ManageHook
windowRules = composeAll [
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
  ]

-- Opacity hook
opacityHook :: EnvConfig -> ManageHook
opacityHook EnvConfig{default_opacity=opac} = composeAll [
    resource =? "kitty" --> makeTransparent
  ] where
      makeTransparent = doSetOpacity (fromIntegral opac / 100.0)

doSetOpacity :: Float -> ManageHook
doSetOpacity opacity = ask >>= \w -> liftX (setWindowOpacity w opacity) >> doF id
