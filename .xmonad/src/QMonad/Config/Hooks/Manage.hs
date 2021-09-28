module QMonad.Config.Hooks.Manage (
  manageHook,
  handleEventHook,
  applyOpacityRule,
) where

import XMonad hiding (manageHook, handleEventHook)
import XMonad.Prelude
import QMonad.Config.Scratchpads (scratchpads, transparentScratchpads)
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat, doRectFloat, composeOne, (-?>))
import XMonad.Util.NamedScratchpad (namedScratchpadManageHook)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import Data.Monoid (All)
import GHC.Float (int2Float)

import QMonad.Config.Env (EnvConfig(..), EnvConfig'(envConfig, globalOpacity))
import QMonad.Lib.Window.Opacity (setWindowOpacity)

wmName = stringProperty "WM_NAME"
wmRole = stringProperty "WM_WINDOW_ROLE"

doCenteredFloat = doRectFloat $ W.RationalRect 0.25 0.25 0.5 0.5

manageHook :: EnvConfig -> ManageHook
manageHook conf = namedScratchpadManageHook (scratchpads conf) <+> windowRules <+> opacityHook

handleEventHook :: Event -> X All
handleEventHook PropertyEvent { ev_event_type = t, ev_atom = a, ev_window = win }
            | t == propertyNotify && a == wM_CLASS = do
  applyOpacityRule win
  return $ All True
handleEventHook _ = return $ All True

handleEvent :: XConfig l -> ManageHook
handleEvent _ = windowRules <+> opacityHook

applyOpacityRule :: Window -> X()
applyOpacityRule win = do
  mh <- asks (handleEvent . config)
  g <- appEndo <$> userCodeDef (Endo id) (runQuery mh win)
  windows g

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
opacityHook :: ManageHook
opacityHook = composeOne $
    [ resource =? r -?> makeTransparent | r <- appNames ]
    ++ [ className =? c -?> makeTransparent | c <- classNames ]
  where makeTransparent = doSetOpacity Nothing
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

doSetOpacity :: Maybe Float -> ManageHook
doSetOpacity opacity = do
  ask >>= \w -> liftX (setOpacity w opacity) >> doF id

setOpacity :: Window -> Maybe Float -> X()
setOpacity w (Just opac) = setWindowOpacity w (opac / 100.0)
setOpacity w Nothing = do
  opac <- XS.gets globalOpacity
  setWindowOpacity w (int2Float opac / 100.0)
