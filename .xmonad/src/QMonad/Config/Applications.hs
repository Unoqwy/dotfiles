module QMonad.Config.Applications (keybindings, spawnTermWithClass) where

import XMonad
import Data.Char (toLower)
import qualified Data.Map as M

import XMonad.Actions.Submap (submap)
import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)

import qualified QMonad.Shared.Theme as T

raiseOrRunOn :: String -> WorkspaceId -> X()
raiseOrRunOn cmd w = raiseMaybe (spawnOn w (map toLower cmd)) (className =? cmd)

runOrRaiseCmd :: String -> X()
runOrRaiseCmd cmd = runOrRaise (map toLower cmd) (className =? cmd)

---------------------- dmenu ----------------------
dmenuParams = " -nb '" ++ T.bgColor ++ "' -nf '" ++ T.fgColor ++ "'"
           ++ " -sb '" ++ T.primaryColor ++ "' -sf '" ++ T.fgOnPrimColor ++ "'"
           ++ " -fn 'Jetbrains Mono:size=10:antialias=true:hinting=true'"

-------------------- Launchers --------------------
spawnTermWithClass :: String -> Maybe String -> String
spawnTermWithClass cls Nothing = "alacritty --class " ++ cls
spawnTermWithClass cls (Just cmd) = "alacritty --class " ++ cls ++ " -e " ++ cmd

calculator = "rofi -show calc -modi calc -no-show-match -no-sort"
clipboardHistory = "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"

keybindings conf@XConfig{XMonad.modMask = modm} = M.fromList [
    ((modm, xK_Return), spawn $ XMonad.terminal conf)

  , ((modm, xK_p), spawn $ "dmenu_run" ++ dmenuParams)
  , ((modm, xK_c), spawn calculator)

  , ((modm, xK_o), submap . M.fromList $ [
        ((0, xK_s), raiseOrRunOn "Spotify" "1")
      , ((0, xK_f), raiseOrRunOn "firefox" "2")
      , ((0, xK_d), runOrRaiseCmd "discord")

      , ((0, xK_w), runOrRaiseCmd "choose-wallpaper")
      , ((0, xK_a), runOrRaiseCmd "rofi-primary-sink")
      , ((0, xK_h), spawn clipboardHistory)
      ])

  -- Special keys
  , ((modm, xK_Print), spawn "flameshot gui")
  ]
