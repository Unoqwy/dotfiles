module QMonad.Config.QwmhDesktops (
  qwmh,
  setDesktopsProperty,
) where

import XMonad
import XMonad.Prelude
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import qualified XMonad.StackSet as W

import Data.List (find)
import Data.Bits (shiftL, (.|.))
import Data.Sequence (fromList, foldlWithIndex, mapWithIndex)
import Codec.Binary.UTF8.String (encode)

import qualified QMonad.Config.Aggregate as A

qwmh :: XConfig a -> XConfig a
qwmh c = c { logHook = logHook' <+> logHook c }

logHook' :: X ()
logHook' = do
  setDesktopsProperty

setDesktopsProperty :: X ()
setDesktopsProperty = do
  aggregateWs <- A.workspaces
  currentWs <- gets (W.currentTag . windowset)
  let Just curAggregate = find (\ws -> A.tag ws == currentWs) aggregateWs
  let names = map A.name aggregateWs
  let bins = map getAggregateBin aggregateWs
  withDisplay $ \dpy -> do
    root <- asks theRoot
    atmTyStr <- getAtom "UTF8_STRING"
    atmNames <- getAtom "_QDE_DESKTOP_NAMES"
    let names' = map fi $ concatMap ((++[0]) . encode) names
    io $ changeProperty8 dpy root atmNames atmTyStr propModeReplace names'
    atmBins <- getAtom "_QDE_DESKTOP_BINS"
    io $ changeProperty8 dpy root atmBins cARDINAL propModeReplace (fmap fi bins)
    atmCur <- getAtom "_QDE_CURRENT_DESKTOP"
    io $ changeProperty32 dpy root atmCur cARDINAL propModeReplace [fi . A.idx $ curAggregate]

getAggregateBin :: A.WorkspaceAggregate -> Int
getAggregateBin ws = do
  let bits = [ A.visible
             , A.empty
             , A.hasMinimized
             , A.focal ]
  let bits' = mapWithIndex (\idx v -> fromEnum (v ws) `shiftL` idx)
            $ fromList bits
  fi $ foldl (.|.) 0 bits'
