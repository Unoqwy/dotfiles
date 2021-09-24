module QMonad.Config.Scratchpads (scratchpads) where

import XMonad
import XMonad.Util.NamedScratchpad (NamedScratchpad(NS), customFloating)
import qualified XMonad.StackSet as W

import QMonad.Config.Env (EnvConfig)

import qualified QMonad.Config.Applications as A

-- Named scratchpads
scratchpads :: EnvConfig -> [NamedScratchpad]
scratchpads conf = [
      NS "floaterm" (A.spawnTermWithClass conf "floaterm" Nothing) (resource =? "floaterm") float
    , NS "floaterm-min" (A.spawnTermWithClass conf "floaterm-min" Nothing) (resource =? "floaterm-min") floatMin
    , NS "quicksearch" "vimb --name quicksearch" (resource =? "quicksearch") float
    , NS "filexplorer" (A.spawnTermWithClass conf "floatfe" (Just "xplr")) (resource =? "floatfe") float
  ] where
      float = customFloating $ W.RationalRect 0.1 0.1 0.8 0.8
      floatMin = customFloating $ W.RationalRect 0.15 0.15 0.7 0.7
