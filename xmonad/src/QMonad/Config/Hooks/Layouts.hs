module QMonad.Config.Hooks.Layouts (
  layoutHook,
) where

import XMonad hiding (layoutHook)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.Renamed (renamed, Rename(..))
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

import QMonad.Config.Layout.FocalWindow (focalWindow)
import QMonad.Config.Layout.ForeignLayout (foreignLayout)

import Data.List (find)
import Data.Maybe (isJust)

-- Layout hook
windowSpacing = 4 `div` 2
screenSpacing = [ 6 - windowSpacing -- Top
                , 6 - windowSpacing -- Bottom
                , 6 - windowSpacing -- Right
                , 6 - windowSpacing -- Left
                ]

mkGaps = spacingRaw False
  (Border (head screenSpacing) (screenSpacing!!1) (screenSpacing!!2) (screenSpacing!!3)) True
  (Border windowSpacing windowSpacing windowSpacing windowSpacing) True

layouts =
      (avoidStruts . foreignLayout $
        focalWindow (
              renamed [Replace "V"] vtall
          ||| renamed [Replace "H"] htall
        )
        ||| renamed [Replace "F"] full
      )
      ||| renamed [Replace "X"] fullScreen
  where
    defaultIncr = 5/100
    vtall = mkGaps $ Mirror $ Tall 2 defaultIncr (2/3)
    htall = mkGaps $ Tall 1 defaultIncr (1/2)
    full  = avoidStruts . mkGaps . noBorders $ Full
    fullScreen = noBorders Full

layoutHook = renamed [CutLeft 9] . minimize . boringWindows . smartBorders $ layouts
