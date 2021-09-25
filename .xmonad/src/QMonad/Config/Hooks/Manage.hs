module QMonad.Config.Hooks.Manage (
  manageHook,
  handleEventHook,
) where

import XMonad hiding (manageHook, handleEventHook)
import XMonad.Prelude
import QMonad.Config.Scratchpads (scratchpads, transparentScratchpads)
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat, doRectFloat, composeOne, (-?>))
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import Data.Monoid (All)

import QMonad.Config.Env (EnvConfig(..), EnvConfig'(envConfig))
import QMonad.Lib.Window.Opacity (setWindowOpacity)

wmName = stringProperty "WM_NAME"
wmRole = stringProperty "WM_WINDOW_ROLE"

doCenteredFloat = doRectFloat $ W.RationalRect 0.25 0.25 0.5 0.5

manageHook :: EnvConfig -> ManageHook
manageHook conf = namedScratchpadManageHook (scratchpads conf) <+> windowRules <+> opacityHook conf

handleEventHook :: Event -> X All
handleEventHook PropertyEvent { ev_event_type = t, ev_atom = a, ev_window = win }
            | t == propertyNotify && a == wM_CLASS = do
  conf <- XS.gets envConfig
  mh <- asks (handleEvent conf . config)
  g <- appEndo <$> userCodeDef (Endo id) (runQuery mh win)
  windows g
  return $ All True
handleEventHook _ = return $ All True

handleEvent :: EnvConfig -> XConfig l -> ManageHook
handleEvent conf _ = windowRules <+> opacityHook conf

windowRules :: ManageHook
windowRules = composeAll [
    isDialog --> doF W.swapUp

  -- Organized applications
  , className =? "Spotify" --> doShift "1"
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
opacityHook :: EnvConfig -> ManageHook
opacityHook EnvConfig{default_opacity=opac} = composeOne $
    [ resource =? r -?> makeTransparent | r <- appNames ]
    ++ [ className =? c -?> makeTransparent | c <- classNames ]
  where makeTransparent = doSetOpacity (fromIntegral opac / 100.0)
        appNames = [
            "jetbrains-idea-ce"
          , "spotify"
          , "discord"
          ] ++ transparentScratchpads
        classNames = [
            "Alacritty"
          , "kitty"
          , "org.wezfurlong.wezterm"
          ]

doSetOpacity :: Float -> ManageHook
doSetOpacity opacity = ask >>= \w -> liftX (setWindowOpacity w opacity) >> doF id
