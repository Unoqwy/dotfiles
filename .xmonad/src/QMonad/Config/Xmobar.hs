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

-- Imports/ Shared
import qualified QMonad.Shared.Theme as T

xmobarStrip :: String -> String
xmobarStrip = xmobarStripTags ["fc","icon","action","box"]

cwrap :: WorkspaceId -> String -> String -> String -> String
cwrap wks a b c = wrap a b (wrap ("<action=`xdotool key super+" ++ wks ++ "`><fn=1> </fn>") "<fn=1> </fn></action>" c)

-- Goal of this log hook:
--   Send the workspaces to xmobar and read it there using the unsafe stdin reader
--   Bind the title + layout to a FIFO so it can be severed from workspaces
wsLogHook xmobarStdin = workspaceMasksPP (def {
      ppOutput  = hPutStrLn xmobarStdin
    , ppOrder   = \(ws:_) -> [ws]
    , ppWsSep   = ""
  }) (def {
      wsppCurrent = \(WorkspaceMask w visible n) _ -> if visible
        then cwrap w ("<box type=Top width=1 offset=C10 color=" ++ T.primaryColor ++ "><fc=" ++ T.primaryColor ++ ">") "</fc></box>" n
        else cwrap w ("<box type=Top width=1 offset=C10 color=" ++ T.secondColor ++ "><fc=" ++ T.secondColor ++ ">") "</fc></box>" n
    , wsppHidden  = \(WorkspaceMask w visible n) _ -> if visible
        then cwrap w "" "" n else ""
    , wsppHiddenNoWindows  = \(WorkspaceMask w visible n) _ -> if visible
        then cwrap w "<fc=#49464e>" "</fc>" n else ""
  }) >>= dynamicLogWithPP

infoLogHook infoPipe = dynamicLogWithPP def {
      ppOutput = hPutStrLn infoPipe
    , ppOrder  = \(_:l:t:_) -> [l,t]

    , ppLayout = wrap ("<box type=Bottom width=2 color=" ++ T.secondColor ++ "><fc=" ++ T.secondColor ++ ">") "</fc></box>"
    , ppTitle  = wrap ("<fc=" ++ T.darkFgColor ++ ">") "</fc>" . xmobarStrip . shorten 40
    , ppSep    = " "
  }

xmobarLogHook xmobarStdin infoPipe = wsLogHook xmobarStdin <+> infoLogHook infoPipe
