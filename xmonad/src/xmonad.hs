import XMonad (xmonad, def, mod4Mask, extensionType)
import qualified XMonad
import XMonad.Util.Run (spawnPipe)

import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageDocks (docks)
import qualified XMonad.Util.ExtensibleState as XS

import GHC.IO.Handle.FD (openFileBlocking)
import System.IO (Handle, IOMode(WriteMode), hSetBuffering, BufferMode(LineBuffering))
import qualified Data.Map as M

import QMonad.Config.Hooks.Layouts (layoutHook)
import QMonad.Config.Keybindings (keybindings)
import QMonad.Config.Xmobar (xmobarLogHook)
import QMonad.Config.Env (EnvConfig(..), EnvState(..), loadEnvConfig)
import QMonad.Config.Hooks.General (hooks)

import qualified QMonad.Shared.Theme as T
import Data.Typeable (typeOf)

-- Open and prepare a Handle to write to a named pipe
getFIFOHandle :: String -> IO Handle
getFIFOHandle path = do
    handle <- openFileBlocking path WriteMode
    hSetBuffering handle LineBuffering
    return handle

-- Main
main :: IO()
main = do
  xmobarStdin <- spawnPipe "cd $XMONAD/bin/xmobar && ./xmobar"
  infoPipe <- getFIFOHandle "/tmp/xmonad-info"

  conf <- loadEnvConfig
  xmonad $ (ewmh . docks . hooks conf) def {
      XMonad.terminal = terminal conf
    , XMonad.modMask = mod4Mask
    , XMonad.workspaces = map show [0..9]

    , XMonad.borderWidth = 1
    , XMonad.focusedBorderColor = T.fgColor
    , XMonad.normalBorderColor = T.bgColor

    , XMonad.keys = keybindings conf
    , XMonad.startupHook = XS.put $ EnvState {
          envConfig = conf,
          globalOpacity = default_opacity conf,
          colorTemp = 6500
        }
    , XMonad.layoutHook = layoutHook
    , XMonad.logHook = xmobarLogHook xmobarStdin infoPipe
    }