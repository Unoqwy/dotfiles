module QMonad.Config.Xmobar (
    xmobarLogHook,

    wsLogHook,
    infoLogHook,
) where

-- Imports/ Base
import XMonad
import XMonad.Util.Run (hPutStrLn)

-- Imports/ Lib
import QMonad.Lib.WorkspaceMasks
import XMonad.Hooks.DynamicLog (
        PP(..), dynamicLogWithPP,
        wrap, shorten, xmobarStripTags,
    )

import XMonad.Util.Minimize (Minimized(minimizedStack))
import qualified XMonad.Util.ExtensibleState as XS
import System.IO (Handle)
import qualified XMonad.StackSet as W

import Data.List (filter)

-- Imports/ Shared
import qualified QMonad.Shared.Theme as T

xmobarStrip :: String -> String
xmobarStrip = xmobarStripTags ["fc","icon","action","box"]

cwrap :: WorkspaceId -> String -> String -> String -> String
cwrap wks a b c = wrap a b (wrap ("<action=`xdotool key super+" ++ wks ++ "`><fn=1> </fn>") "<fn=1> </fn></action>" c)

-- Goal of this log hook:
--   Send the workspaces to xmobar and read it there using the unsafe stdin reader
--   Bind the title + layout to a FIFO so it can be severed from workspaces
--
-- If all windows in a hidden workspace are minimized, display it as a hiddenNoWindows ws.
wsLogHook :: Handle -> (WorkspaceId -> Bool) -> X()
wsLogHook xmobarStdin lookup = workspaceMasksPP (def {
      ppOutput  = hPutStrLn xmobarStdin
    , ppOrder   = \(ws:_) -> [ws]
    , ppWsSep   = ""
  }) (def {
      wsppCurrent = \(WorkspaceMask w visible n) _ -> if visible
        then cwrap w ("<box type=Top width=1 offset=C10 color=" ++ T.primaryColor ++ "><fc=" ++ T.primaryColor ++ ">") "</fc></box>" n
        else cwrap w ("<box type=Top width=1 offset=C10 color=" ++ T.secondColor ++ "><fc=" ++ T.secondColor ++ ">") "</fc></box>" n
    , wsppHidden  = \(WorkspaceMask w visible n) _ -> if visible
        then if lookup w then cwrap w "" "" n else cwrap w "<fc=#49464e>" "</fc>" n
        else ""
    , wsppHiddenNoWindows  = \(WorkspaceMask w visible n) _ -> if visible
        then cwrap w "<fc=#49464e>" "</fc>" n else ""
  }) >>= dynamicLogWithPP

infoLogHook :: Handle -> Bool -> X()
infoLogHook infoPipe showTitle = do
  dynamicLogWithPP def {
      ppOutput = hPutStrLn infoPipe
    , ppOrder  = \(_:l:t:_) -> [l,t]

    -- drop 9 strips "Minimize " prefix from layout
    , ppLayout = wrap ("<box type=Bottom width=2 color=" ++ T.secondColor ++ "><fc=" ++ T.secondColor ++ ">") "</fc></box>" . drop 9
    , ppTitle = \title -> if showTitle then shorten 40 . xmobarStrip $ wrap ("<fc=" ++ T.darkFgColor ++ ">") "</fc>" title else ""
    , ppSep  = " "
  }

xmobarLogHook :: Handle -> Handle -> X()
xmobarLogHook xmobarStdin infoPipe = do
  minimized <- XS.gets minimizedStack
  wset <- gets windowset

  let allMinimizedWS = map W.tag $
        filter (\w -> not $ any (`notElem` minimized) (W.integrate' $ W.stack w)) (W.workspaces wset)
  wsLogHook xmobarStdin (`notElem` allMinimizedWS)

  -- don't show title in status bar if all windows are minimized
  let currentWS = W.workspace $ W.current wset
  infoLogHook infoPipe (any (`notElem` minimized) (W.integrate' $ W.stack currentWS))
