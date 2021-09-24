module QMonad.Config.Hooks.Layouts (layoutHook) where

import XMonad hiding (layoutHook)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Spacing (spacingRaw, Border(..))

-- Layouts
windowSpacing = 4 `div` 2
screenSpacing = [ 6 - windowSpacing -- Top
                , 6 - windowSpacing -- Bottom
                , 6 - windowSpacing -- Right
                , 6 - windowSpacing -- Left
                ]

mkGaps = spacingRaw False
  (Border (head screenSpacing) (screenSpacing!!1) (screenSpacing!!2) (screenSpacing!!3)) True
  (Border windowSpacing windowSpacing windowSpacing windowSpacing) True

layouts = named "V 2*|1" vtall
      ||| named "H 1|1" htall
      ||| named "Full" full
      ||| named "" fullScreen
  where
    defaultIncr = 5/100
    htall = avoidStruts . mkGaps $ Tall 1 defaultIncr (1/2)
    vtall = avoidStruts . mkGaps $ Mirror $ Tall 2 defaultIncr (2/3)
    full  = avoidStruts . mkGaps . noBorders $ Full
    fullScreen = noBorders Full

layoutHook = minimize . boringWindows . smartBorders $ layouts
