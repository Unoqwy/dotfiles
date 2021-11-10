module QMonad.Config.Applications (terminal, keybindings, spawnTermWithClass) where

import XMonad
import Data.Char (toLower)
import qualified Data.Map as M

import XMonad.Actions.Submap (submap)
import XMonad.Actions.SpawnOn (spawnOn)
import XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)

import qualified QMonad.Config.Env as Env

import qualified QMonad.Shared.Theme as T

raiseOrRunOn :: String -> WorkspaceId -> X()
raiseOrRunOn cmd w = raiseMaybe (spawnOn w (map toLower cmd)) (className =? cmd)

runOrRaiseCmd :: String -> X()
runOrRaiseCmd cmd = runOrRaise (map toLower cmd) (className =? cmd)

---------------------- dmenu ----------------------
dmenuParams = " -nb '" ++ T.bgColor ++ "' -nf '" ++ T.fgColor ++ "'"
           ++ " -sb '" ++ T.primaryColor ++ "' -sf '" ++ T.fgOnPrimColor ++ "'"
           ++ " -fn 'Jetbrains Mono:size=10:antialias=true:hinting=true'"

-- Spawn a terminal with a class, the terminal in use must support '--class' and '-e' arguments.
spawnTermWithClass :: Env.EnvConfig -> String -> Maybe String -> String
spawnTermWithClass Env.EnvConfig{Env.terminal = term} cls Nothing = term ++ " --class " ++ cls
spawnTermWithClass Env.EnvConfig{Env.terminal = term} cls (Just cmd) = term ++ " --class " ++ cls ++ " -e " ++ cmd

-- Rofi launchers
calculator = "rofi -show calc -modi calc -no-show-match -no-sort"
clipboardHistory = "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"

keybindings conf@XConfig{XMonad.modMask = modm} = M.fromList [
    ((modm, xK_Return), spawn $ XMonad.terminal conf)
  , ((modm, xK_p), spawn $ "dmenu_run" ++ dmenuParams)

  , ((modm, xK_o), submap . M.fromList $ [
        ((0, xK_s), raiseOrRunOn "Spotify" "1")
      , ((0, xK_f), raiseOrRunOn "firefox" "2")
      , ((0, xK_d), runOrRaiseCmd "discord")

      , ((0, xK_c), spawn calculator)
      , ((0, xK_h), spawn clipboardHistory)

      , ((0, xK_w), spawn "choose-wallpaper")
      , ((0, xK_k), spawn "choose-kbd-layout")
      , ((0, xK_a), spawn "choose-primary-sink")
      ])

  -- Special keys
  , ((modm, xK_Print), spawn "flameshot gui")
  , ((modm, xK_Delete), spawn "setxkbmap us && notify-send 'Layout reset'")
  ]
