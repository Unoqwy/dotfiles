module QMonad.Config.Hooks.General (hooks) where

import XMonad ((<+>), XConfig, X, liftIO, spawn)
import qualified XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Hooks.InsertPosition (insertPosition, Focus(..), Position(..))
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook)

import System.Environment (lookupEnv, setEnv)
import qualified Data.Bifunctor

import QMonad.Config.Hooks.Layouts (layoutHook)
import qualified QMonad.Config.Hooks.Manage as Hooks.Manage
import QMonad.Config.Env (EnvConfig)

import QMonad.Lib.WorkspaceMasks (setWorkspaceMask)

-- Startup hook
setupDefaultWorkspaces :: X()
setupDefaultWorkspaces = do
    mapM_ (\w -> setWorkspaceMask w Nothing (Just False)) hidden
    mapM_ (\t -> setWorkspaceMask (fst t) (Just $ snd t) Nothing) named
    addHiddenWorkspace "DND"
  where hidden = map show ([0, 1, 5] ++ [7..9]) ++ ["DND", "NSP"]
        named  = map (Data.Bifunctor.first show) [
              (0, "0")
            , (1, "music")
            , (2, "web")
            , (3, "dev")
            , (4, "main")
            , (5, "any")
            , (6, "chat")
            , (7, "ts")
            , (8, "tm")
            , (9, "media")
            ]

startupHook :: X()
startupHook = do
  -- start daemons
  spawnOnce "flameshot &"
  spawnOnce "greenclip daemon &"
  spawnOnce "dunst &"
  spawnOnce "unread-bell &"

  -- compositor
  spawnOnce "picom --config $XDG_CONFIG_HOME/picom/picom.conf &"

  xmonad_started <- liftIO $ lookupEnv "XMONAD_STARTED"
  case xmonad_started of
    Just "1" -> spawn "notify-send 'xmonad' 'Restart OK'"
    _ -> do
      XMonad.windows $ W.greedyView "4"
      liftIO $ setEnv "XMONAD_STARTED" "1"

  setupDefaultWorkspaces

-- Main hooks
hooks :: EnvConfig -> XConfig a -> XConfig a
hooks conf xconf = xconf {
    XMonad.startupHook = startupHook <+> XMonad.startupHook xconf
  , XMonad.handleEventHook = Hooks.Manage.handleEventHook <+> windowedFullscreenFixEventHook
  , XMonad.manageHook  = insertPosition Below Newer <+> Hooks.Manage.manageHook conf
  }
