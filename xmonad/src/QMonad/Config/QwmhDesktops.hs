module QMonad.Config.QwmhDesktops (
  qwmh,
  setDesktopsProperty,
) where

import XMonad
import XMonad.Prelude
import XMonad.Util.WorkspaceCompare (getSortByIndex)

import Data.Bits (shiftL, (.|.))
import Data.Sequence (fromList, foldlWithIndex, mapWithIndex)

import qualified QMonad.Config.Aggregate as A

qwmh :: XConfig a -> XConfig a
qwmh c = c { logHook = logHook' <+> logHook c }

logHook' :: X ()
logHook' = do
  setDesktopsProperty

setDesktopsProperty :: X ()
setDesktopsProperty = do
  aggregateWs <- A.workspaces
  let bins = map getAggregateBin aggregateWs
  withDisplay $ \dpy -> do
    root <- asks theRoot
    atom <- getAtom "Q_DESKTOPS"
    io $ changeProperty8 dpy root atom cARDINAL propModeReplace (fmap fi bins)

getAggregateBin :: A.WorkspaceAggregate -> Int
getAggregateBin ws = do
  let bits = [ A.visible
             , A.empty
             , A.hasMinimized
             , A.focal ]
  let bits' = mapWithIndex (\idx v -> fromEnum (v ws) `shiftL` idx)
            $ fromList bits
  fi $ foldl (.|.) 0 bits'
