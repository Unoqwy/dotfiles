{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import XMonad ((<+>), xmonad, def, mod4Mask)
import qualified XMonad
import XMonad.Util.Run

import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.InsertPosition (insertPosition, Focus(..), Position(..))
import XMonad.Hooks.ManageDocks (docks)

import GHC.IO.Handle.FD (openFileBlocking)
import System.IO (Handle, IOMode(WriteMode), hSetBuffering, BufferMode(LineBuffering))

import QMonad.Config.Keybindings (keybindings)
import QMonad.Config.Xmobar (xmobarLogHook)
import QMonad.Config.Env (EnvConfig(..), loadEnvConfig)

import qualified QMonad.Config.Hooks as Hooks
import qualified QMonad.Shared.Theme as T

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

  xmonad $ (ewmh . docks) def {
      XMonad.terminal = terminal conf
    , XMonad.modMask = mod4Mask
    , XMonad.workspaces = map show [0..9]

    , XMonad.borderWidth = 1
    , XMonad.focusedBorderColor = T.fgColor
    , XMonad.normalBorderColor = T.bgColor

    , XMonad.keys = keybindings conf
    , XMonad.layoutHook = Hooks.layoutHook
    , XMonad.startupHook = Hooks.startupHook
    , XMonad.manageHook = insertPosition Below Newer <+> Hooks.manageHook conf
    , XMonad.logHook = xmobarLogHook xmobarStdin infoPipe <+> Hooks.logHook
    }

