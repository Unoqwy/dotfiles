module QMonad.Config.Util (
  doRectFloatSeventy,
  doRectFloatEighty,
) where

import XMonad (ManageHook)
import XMonad.Hooks.ManageHelpers (doRectFloat)
import qualified XMonad.StackSet as W

doRectFloatSeventy, doRectFloatEighty :: ManageHook
doRectFloatSeventy = doRectFloat $ W.RationalRect 0.15 0.15 0.7 0.7
doRectFloatEighty = doRectFloat $ W.RationalRect 0.1 0.1 0.8 0.8
