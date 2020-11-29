{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import XMonad
import XMonad.Util.Run

import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.InsertPosition (insertPosition, Focus(..), Position(..))
import XMonad.Hooks.ManageDocks (docks)
import XMonad.Layout.NoBorders (smartBorders)

import GHC.IO.Handle.FD (openFileBlocking)
import System.IO (Handle, IOMode(WriteMode), hSetBuffering, BufferMode(LineBuffering))

import QMonad.Config.Keybindings (keybindings)
import QMonad.Config.Xmobar (xmobarLogHook)
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
  xmobarStdin <- spawnPipe "cd $XMONAD/bin/xmobar && (./build &>/dev/null || 1) && ./xmobar"
  infoPipe <- getFIFOHandle "/tmp/xmonad-info"

  xmonad $ (ewmh . docks) def {
      terminal           = "alacritty"
    , modMask            = mod4Mask
    , workspaces         = map show [0..9]

    , borderWidth        = 1
    , focusedBorderColor = T.primaryColor
    , normalBorderColor  = T.bgColor

    , keys               = keybindings
    , layoutHook = smartBorders Hooks.layouts
    , startupHook = Hooks.startupHook
    , manageHook = insertPosition End Newer <+> Hooks.manageHook
    , logHook = xmobarLogHook xmobarStdin infoPipe <+> Hooks.logHook
    }

