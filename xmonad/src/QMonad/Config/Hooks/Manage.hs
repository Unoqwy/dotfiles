module QMonad.Config.Hooks.Manage (
  manageHook,
  handleEventHook,
  applyOpacityRule,
) where

import XMonad hiding (manageHook, handleEventHook)
import XMonad.Prelude
import QMonad.Config.DoNotDisturb (isDND)
import qualified QMonad.Config.Scratchpads (transparentScratchpads, manageHook')
import XMonad.Hooks.ManageHelpers (isDialog, doCenterFloat, doRectFloat, composeOne, (-?>), isInProperty)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import Data.Monoid (All)
import GHC.Float (int2Float)

import QMonad.Config.Env (EnvConfig(..), EnvState(envConfig, globalOpacity))
import QMonad.Lib.Window.Opacity (setWindowOpacity)

-- Utils
wmName = stringProperty "WM_NAME"
wmRole = stringProperty "WM_WINDOW_ROLE"

isNotification :: Query Bool
isNotification = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_NOTIFICATION"

doX :: (Window -> X()) -> ManageHook
doX fn = ask >>= \w -> liftX (fn w) >> doF id

-- Centered floats
doCenteredFloat = doRectFloat $ W.RationalRect 0.25 0.25 0.5 0.5

-- Hooks
manageHook :: EnvConfig -> ManageHook
manageHook conf = QMonad.Config.Scratchpads.manageHook' conf <+> manageHook'

handleEventHook :: Event -> X All
handleEventHook PropertyEvent { ev_event_type = t, ev_atom = a, ev_window = win }
            | t == propertyNotify && a == wM_CLASS = do
  forceRunHook win
  return $ All True
handleEventHook _ = return $ All True

forceRunHook, applyOpacityRule :: Window -> X()
forceRunHook win = do
  g <- appEndo <$> userCodeDef (Endo id) (runQuery manageHook' win)
  windows g
applyOpacityRule = forceRunHook

-- rules
manageHook' :: ManageHook
manageHook' = windowRules
          -- <+> dndRules
          <+> opacityHook

windowRules :: ManageHook
windowRules = composeAll [
    isDialog --> doF W.swapUp

  -- Organized applications
  , className =? "Spotify" --> doShift "1"
  , className =? "discord" --> doShift "6"
  , className =? "TeamSpeak 3" --> doShift "7"
  , resource =? "notion-app" <||> className =? "Notion" --> doShift "8"
  , className =? "media-app" --> doShift "9"

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

-- DND rules
dndRules :: ManageHook
dndRules = composeOne $ (q' -?> idHook) : [ q -?> doShift "DND" | q <- hide ]
  where q' = liftX $ not <$> isDND
        hide = []

-- Opacity rules
opacityHook :: ManageHook
opacityHook = composeOne $
    [ resource =? r -?> makeTransparent | r <- appNames ]
    ++ [ className =? c -?> makeTransparent | c <- classNames ]
    ++ [ fmap ("wsterm-" `isPrefixOf`) resource -?> makeTransparent ]
  where makeTransparent = doSetOpacity Nothing
        t' = QMonad.Config.Scratchpads.transparentScratchpads
        appNames = [
            "jetbrains-idea-ce"
          , "spotify"
          , "discord"
          , "code"
          ] ++ t'
        classNames = [
            "Alacritty"
          , "kitty"
          , "org.wezfurlong.wezterm"
          ]

doSetOpacity :: Maybe Float -> ManageHook
doSetOpacity opacity = doX $ \w -> setOpacity w opacity

setOpacity :: Window -> Maybe Float -> X()
setOpacity w (Just opac) = setWindowOpacity w (opac / 100.0)
setOpacity w Nothing = do
  opac <- XS.gets globalOpacity
  setWindowOpacity w (int2Float opac / 100.0)
