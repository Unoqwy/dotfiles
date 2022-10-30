module QMonad.Config.Hooks.General (hooks, allWorkspaces) where

import XMonad
import XMonad.Prelude
import qualified XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Hooks.InsertPosition (insertPosition, Focus(..), Position(..))
import XMonad.Util.Minimize (Minimized(minimizedStack))
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Hacks (windowedFullscreenFixEventHook)
import qualified XMonad.Layout.BoringWindows as BW
import qualified XMonad.Util.ExtensibleState as XS

import System.Environment (lookupEnv, setEnv)
import qualified Data.Bifunctor

import QMonad.Config.Hooks.Layouts (layoutHook)
import qualified QMonad.Config.Hooks.Manage as Hooks.Manage
import QMonad.Config.Env (EnvConfig)

import QMonad.Lib.WorkspaceMasks (setWorkspaceMask)
import QMonad.Config.Layout.ForeignLayout (ForeignMessage(PopulateBoundWs))

allWorkspaces :: XConfig a -> [(KeySym, WorkspaceId)]
allWorkspaces xconf = [
    (xK_d, "DND")
  , (xK_n, "NSP")
  ] ++ [(k, w) | (w, k) <- zip (XMonad.workspaces xconf) [xK_0 ..]]

-- Startup hook
setupDefaultWorkspaces :: XConfig a -> X()
setupDefaultWorkspaces xconf = do
    mapM_ (\w -> setWorkspaceMask w Nothing (Just False)) hidden
    mapM_ (\t -> setWorkspaceMask (fst t) (Just $ snd t) Nothing) named
    addHiddenWorkspace "DND"
    mapM_ (\wid -> do
        wset <- gets windowset
        let ws = find ((==wid).W.tag) (W.workspaces wset)
        ul <- handleMessage (W.layout . fromJust $ ws) (SomeMessage . PopulateBoundWs $ wid) `catchX` return Nothing
        whenJust ul $ \l' -> modifyWindowSet $ \wset -> do
          let u = W.view wid wset
          W.view (W.currentTag wset) $ u
            { W.current = (W.current u)
            { W.workspace = (W.workspace $ W.current u)
            { W.layout = l' }}}
      ) allWs
  where hidden = map show ([0, 1, 5] ++ [6..9]) ++ ["DND", "NSP"]
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
        allWs = map snd (allWorkspaces xconf)

startupHook' :: XConfig a -> X()
startupHook' xconf = do
  -- start daemons
  spawnOnce "flameshot &"
  spawnOnce "greenclip daemon &"
  spawnOnce "dunst &"
  spawnOnce "unread-bell &"

  -- compositor
  spawnOnce "picom --config $XDG_CONFIG_HOME/picom/picom.conf --experimental-backends &"

  xmonad_started <- liftIO $ lookupEnv "XMONAD_STARTED"
  case xmonad_started of
    Just "1" -> spawn "notify-send 'xmonad' 'Restart OK'"
    _ -> do
      XMonad.windows $ W.greedyView "4"
      liftIO $ setEnv "XMONAD_STARTED" "1"

  setupDefaultWorkspaces xconf

-- Main hooks
hooks :: EnvConfig -> XConfig a -> XConfig a
hooks conf xconf = xconf {
    XMonad.startupHook = startupHook' xconf <+> XMonad.startupHook xconf
  , XMonad.handleEventHook = Hooks.Manage.handleEventHook <+> windowedFullscreenFixEventHook
  , XMonad.manageHook = insertPosition Below Newer <+> Hooks.Manage.manageHook conf
  , XMonad.logHook = logHook' <+> XMonad.logHook xconf
  }

logHook' :: X()
logHook' = withFocused checkFocus

-- Focus
fixFocus :: X()
fixFocus = do
  wset <- gets windowset
  let stack = W.integrate' . W.stack . W.workspace . W.current $ wset
  minimized <- XS.gets minimizedStack
  when (any (`notElem` minimized) stack) BW.focusDown

checkFocus :: Window -> X()
checkFocus w = flip when fixFocus . (w `elem`) =<< XS.gets minimizedStack
