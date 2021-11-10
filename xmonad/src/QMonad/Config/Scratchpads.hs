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
