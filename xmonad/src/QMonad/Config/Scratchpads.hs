module QMonad.Config.Scratchpads (
  manageHook',
  scratchpads,
  transparentScratchpads,
  oneScratchpadAction,
  wsScratchpadTerminal,
) where

import XMonad
import XMonad.Prelude
import XMonad.Hooks.ManageHelpers
import XMonad.Util.NamedScratchpad (NamedScratchpad(..), namedScratchpadAction, namedScratchpadManageHook)
import qualified XMonad.StackSet as W

import QMonad.Config.Env (EnvConfig)
import QMonad.Config.Util

import qualified QMonad.Config.Applications as A

manageHook' :: EnvConfig -> ManageHook
manageHook' conf = namedScratchpadManageHook (scratchpads conf) <+> wsScratchpadManageHook

-- Named scratchpads
scratchpads :: EnvConfig -> [NamedScratchpad]
scratchpads conf = [
      NS "floaterm" (A.spawnTermWithClass conf "floaterm" Nothing) (resource =? "floaterm") doRectFloatEighty
    , NS "floaterm-min" (A.spawnTermWithClass conf "floaterm-min" Nothing) (resource =? "floaterm-min") doRectFloatSeventy
    , NS "quicksearch" "vimb --name quicksearch" (resource =? "quicksearch") doRectFloatEighty
    , NS "filexplorer" (A.spawnTermWithClass conf "floatfe" (Just "xplr")) (resource =? "floatfe") doRectFloatEighty
    , NS "dev-dashboard" "spawn-dev-dashboard" (fmap ("dev-dashboard-" `isPrefixOf`) className) doPlaceDevDashboard
  ]

transparentScratchpads :: [String]
transparentScratchpads = ["floaterm", "floaterm-min", "floatfe"]

-- Scratchpad special actions
oneScratchpadAction :: NamedScratchpad -> X()
oneScratchpadAction s@NS { name = name } = namedScratchpadAction [s] name

wsScratchpadTerminal :: EnvConfig -> WorkspaceId -> NamedScratchpad
wsScratchpadTerminal conf wid = NS c (A.spawnTermWithClass conf c Nothing) (resource =? c) (doF id)
  where c = "wsterm-" ++ wid

wsScratchpadManageHook :: ManageHook
wsScratchpadManageHook = composeOne [ fmap ("wsterm-" `isPrefixOf`) resource -?> doRectFloatSeventy ]

doPlaceDevDashboard :: ManageHook
doPlaceDevDashboard = composeOne [
    className =? "dev-dashboard-tasks" -?> (doRectFloat $ W.RationalRect 0.05 0.1 0.5 0.8)
  , className =? "dev-dashboard-tracker" -?> (doRectFloat $ W.RationalRect 0.55 0.1 0.4 0.8)
  ]
