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
  ]
  ++ -- Customizable scratchpads
  [createQuickViewNS id | id <- map show [1..4]]

createQuickViewNS :: String -> NamedScratchpad
createQuickViewNS id = NS ("quickview-" ++ id) ("spawn-qv " ++ id) (fmap (c `isPrefixOf`) className) (doPlaceFlexibleScratchpad c)
  where c = "qde-qv" ++ id

transparentScratchpads :: [String]
transparentScratchpads = ["floaterm", "floaterm-min"]

-- Scratchpad special actions
oneScratchpadAction :: NamedScratchpad -> X()
oneScratchpadAction s@NS { name = name } = namedScratchpadAction [s] name

wsScratchpadTerminal :: EnvConfig -> WorkspaceId -> NamedScratchpad
wsScratchpadTerminal conf wid = NS c (A.spawnTermWithClass conf c Nothing) (resource =? c) (doF id)
  where c = "wsterm-" ++ wid

wsScratchpadManageHook :: ManageHook
wsScratchpadManageHook = composeOne [ fmap ("wsterm-" `isPrefixOf`) resource -?> doRectFloatSeventy ]

-- Place windows depending on their assigned class name
doPlaceFlexibleScratchpad :: String -> ManageHook
doPlaceFlexibleScratchpad prefix = composeOne [
    className =? (prefix ++ "-main") -?> (doRectFloat $ W.RationalRect 0.1 0.1 0.8 0.8)
  , className =? (prefix ++ "-wide-main") -?> (doRectFloat $ W.RationalRect 0.05 0.1 0.9 0.8)
  , className =? (prefix ++ "-primary") -?> (doRectFloat $ W.RationalRect 0.05 0.1 0.5 0.8)
  , className =? (prefix ++ "-secondary") -?> (doRectFloat $ W.RationalRect 0.55 0.1 0.4 0.8)
  ]
