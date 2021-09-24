module QMonad.Config.Hooks.Manage (
  manageHook,
  handleEventHook,
) where

import XMonad hiding (manageHook, handleEventHook)
import XMonad.Prelude
import QMonad.Config.Scratchpads (scratchpads)
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat, doRectFloat)
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)
import qualified XMonad.StackSet as W

import Data.Monoid (All)

import QMonad.Config.Env (EnvConfig(..))
import QMonad.Lib.Window.Opacity (setWindowOpacity)

wmName = stringProperty "WM_NAME"
wmRole = stringProperty "WM_WINDOW_ROLE"

doCenteredFloat = doRectFloat $ W.RationalRect 0.25 0.25 0.5 0.5

manageHook :: EnvConfig -> ManageHook
manageHook conf = namedScratchpadManageHook (scratchpads conf) <+> windowRules <+> opacityHook

handleEventHook :: Event -> X All
handleEventHook PropertyEvent { ev_event_type = t, ev_atom = a, ev_window = win }
            | t == propertyNotify && a == wM_CLASS = do
  mh <- asks (handleEvent . config)
  g <- appEndo <$> userCodeDef (Endo id) (runQuery mh win)
  windows g
  return $ All True
handleEventHook _ = return $ All True

handleEvent :: XConfig l -> ManageHook
handleEvent _ = windowRules <+> opacityHook

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
  , className =? "Pavucontrol" --> doCenteredFloat
  , className =? "flameshot" <&&> title =? "Configuration" --> doCenterFloat
  , resource =? "sxiv" --> doCenterFloat

  -- Firefox PiP
  , wmName =? "Picture-in-Picture" --> doFloat

  -- Misc
  , resource =? "Godot_ProjectList" --> doCenteredFloat
  , wmRole =? "GtkFileChooserDialog" --> doCenteredFloat
  ]

-- Opacity hook
opacityHook :: ManageHook
opacityHook = composeAll $
    [ resource =? r --> makeTransparent | r <- defaultTransparent ]
  where makeTransparent = doSetOpacity (95 / 100.0)
        defaultTransparent = [
            "kitty"
          , "Alacritty"
          , "org.wezfurlong.wezterm"
          , "jetbrains-idea-ce"
          , "spotify"
          , "discord"
          ]

doSetOpacity :: Float -> ManageHook
doSetOpacity opacity = ask >>= \w -> liftX (setWindowOpacity w opacity) >> doF id
