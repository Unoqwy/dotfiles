module QMonad.Config.IPC (
  MediaControl(..),
  mediaAction,
  StatusBar(..),
  hideStatusBar,
  showStatusBar,
  toggleStatusBar,
) where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS
import GHC.Show (showSpace)
import XMonad.Hooks.DynamicLog (statusBar)

-- Media controls
data MediaControl = PlayPause | MNext | MPrevious

instance Show MediaControl where
  show PlayPause = "PlayPause"
  show MNext = "Next"
  show MPrevious = "Previous"

dbusMediaPlayer :: String -> MediaControl -> X()
dbusMediaPlayer app action = spawn
  $ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2."  ++ show app
  ++ " /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player." ++ show action

mediaAction :: MediaControl -> X()
mediaAction action = do
  dbusMediaPlayer "spotify" action
  dbusMediaPlayer "spotifyd" action

-- Status bar
newtype StatusBar = StatusBar Bool
    deriving (Read, Show, Typeable)

instance ExtensionClass StatusBar where
  initialValue = StatusBar True
  extensionType = PersistentExtension

dbusXmobar :: String -> X()
dbusXmobar action = spawn
  $ "dbus-send --print-reply --dest=org.Xmobar.Control"
  ++ " /org/Xmobar/Control org.Xmobar.Control.SendSignal \"string:" ++ action ++ "\""

hideStatusBar :: X()
hideStatusBar = do
  dbusXmobar "Hide 0"
  XS.put $ StatusBar False

showStatusBar :: X()
showStatusBar = do
  dbusXmobar "Reveal 0"
  XS.put $ StatusBar True

toggleStatusBar :: X()
toggleStatusBar = do
  StatusBar state <- XS.get
  if state
    then hideStatusBar
    else showStatusBar
