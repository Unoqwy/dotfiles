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

import QMonad.Config.Layout.LayoutDescriptionMeta (parseLayoutDescription)

-- Imports/ Shared
import qualified QMonad.Shared.Theme as T
import Control.Monad (when)
import Data.List (find)

xmobarStrip :: String -> String
xmobarStrip = xmobarStripTags ["fc","icon","action","box"]

awrap :: WorkspaceId -> String -> String
awrap wks = wrap ("<action=`xdotool key super+" ++ wks ++ "`><fn=1> </fn>") "<fn=1> </fn></action>"

awrap' :: WorkspaceId -> String -> String -> String -> String
awrap' wks l r name = wrap l r (awrap wks name)

infoBorders :: (Bool, Bool) -> String -> String
infoBorders (hm,hf) s = do
  wrapIf hm ("<box type=Right width=2 align=L maxlen=2 color=" ++ T.brightFgColor ++ ">") "</box>" $
    wrapIf hf ("<box type=Left width=3 align=L maxlen=4 color=" ++ T.focusColor ++ ">") "</box>" s

wrapIf :: Bool -> String -> String -> String -> String
wrapIf True l r m = wrap l r m
wrapIf False _ _ s = s

-- Goal of this log hook:
--   Send the workspaces to xmobar and read it there using the unsafe stdin reader
--   Bind the title + layout to a FIFO so it can be severed from workspaces
--
-- If all windows in a hidden workspace are minimized, display it as a hiddenNoWindows ws.
-- If there is a current focal window, dislay it as a focal ws.
wsLogHook :: Handle -> (WorkspaceId -> Bool) -> (WorkspaceId -> Bool) -> X()
wsLogHook xmobarStdin isEmpty containsMinimized = do
  minimized <- XS.gets minimizedStack
  wset <- gets windowset
  let ib = \wid n -> do
        let Just ws = find (\ws -> W.tag ws == wid) (W.workspaces wset)
        let (meta,_) = parseLayoutDescription [] (description . W.layout $ ws)
        infoBorders (containsMinimized wid, "focal" `elem` meta) n
  workspaceMasksPP (def {
      ppOutput  = hPutStrLn xmobarStdin
    , ppOrder   = \(ws:_) -> [ws]
    , ppWsSep   = ""
  }) (def {
      wsppCurrent = \(WorkspaceMask w visible n) _ ->
          let color = if visible then T.primaryColor else T.secondaryColor
          in awrap' w ("<box type=Top width=1 ml=10 mr=10 color=" ++ color ++ "><fc=" ++ color ++ ">") "</fc></box>" (ib w n)
    , wsppHidden  = \(WorkspaceMask w visible n) _ -> if visible then
          let n' = ib w n
          in if isEmpty w then awrap w n' else awrap' w "<fc=#49464e>" "</fc>" n'
        else ""
    , wsppHiddenNoWindows  = \(WorkspaceMask w visible n) _ -> if visible
        then awrap' w "<fc=#49464e>" "</fc>" n else ""
  }) >>= dynamicLogWithPP

infoLogHook :: Handle -> Bool -> String -> X()
infoLogHook infoPipe showTitle ld = do
  let (meta, ld') = parseLayoutDescription [] ld
  let titleColor = if "focal" `elem` meta then T.focusColor else T.darkFgColor
  dynamicLogWithPP def {
      ppOutput = hPutStrLn infoPipe
    , ppOrder  = \(_:l:t:_) -> [l,t]

    , ppLayout = \_ -> wrap ("<box type=Bottom width=2 color=" ++ T.secondaryColor ++ "><fc=" ++ T.secondaryColor ++ ">") "</fc></box>" ld'
    , ppTitle = \s -> if showTitle
          then wrap ("<fc=" ++ titleColor ++ ">") "</fc>" (shorten 40 . xmobarStrip $ s)
          else ""
    , ppSep  = " "
  }

xmobarLogHook :: Handle -> Handle -> X()
xmobarLogHook xmobarStdin infoPipe = do
  minimized <- XS.gets minimizedStack
  wset <- gets windowset

  let wsWithMinimized = map W.tag $
        filter (\w -> any (`elem` minimized) (W.integrate' $ W.stack w)) (W.workspaces wset)
  let wsWithOnlyMinimized = map W.tag $
        filter (\w -> not $ any (`notElem` minimized) (W.integrate' $ W.stack w)) (W.workspaces wset)
  wsLogHook xmobarStdin (`notElem` wsWithOnlyMinimized) (`elem` wsWithMinimized)

  -- don't show title in status bar if all windows are minimized
  let currentWS = W.workspace $ W.current wset
  infoLogHook infoPipe (any (`notElem` minimized) (W.integrate' $ W.stack currentWS)) (description . W.layout $ currentWS)
