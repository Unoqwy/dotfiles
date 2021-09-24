module QMonad.Config.Keybindings (
    keybindings
) where

import XMonad
import System.Exit (exitSuccess)
import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Actions.Submap (submap)
import Graphics.X11.ExtraTypes.XF86

import XMonad.Layout.Spacing (
    toggleScreenSpacingEnabled, toggleWindowSpacingEnabled
  )
import XMonad.Actions.SwapWorkspaces (swapWithCurrent)
import QMonad.Lib.WorkspaceMasks (
    renameCurrentWorkspace, toggleCurrentWorkspaceVisibility
  )

import XMonad.Util.NamedScratchpad (namedScratchpadAction)

import Control.Monad (when)
import XMonad.Util.Run (runProcessWithInput)
import qualified XMonad.Layout.BoringWindows as BW

import System.Environment

import QMonad.Lib.Window.Minimize (minimizeWindow, maximizeWindow, sortMinimizedWindows, minimizedStack)

import QMonad.Config.Env (EnvConfig(..), localBin)
import QMonad.Config.IPC (MediaControl(..), mediaAction, toggleStatusBar)
import qualified QMonad.Config.Scratchpads as Scratchpads

import qualified QMonad.Config.Applications as A
import qualified QMonad.Config.Prompt as XP

-- Toggles
toggleGaps :: X()
toggleGaps = do
  toggleScreenSpacingEnabled
  toggleWindowSpacingEnabled

-- Minimizing windows
chooseWindowToMaximize :: EnvConfig -> X()
chooseWindowToMaximize conf = do
  minimized <- sortMinimizedWindows
  window <- runProcessWithInput (localBin conf "unhide") (map show minimized) []
  let win = read window :: Window
  when (window /= "") (maximizeWindow win)

minimizeCurrentWindow :: X()
minimizeCurrentWindow = withFocused minimizeWindow <+> BW.focusUp

fixFocus :: X()
fixFocus =
  return ()

checkFocus :: Window -> X()
checkFocus w = do
  minimized <- XS.gets minimizedStack
  when (w `elem` minimized) fixFocus

-- Keybindings
keybindings :: EnvConfig -> XConfig Layout -> M.Map (ButtonMask, KeySym) (X())
keybindings conf xconf@XConfig {XMonad.modMask = modm} = M.fromList ([
  -- Brightness control
    ((modm, xK_Left ), spawn "$XMONAD/bin/brightness -0.1")
  , ((modm, xK_Right), spawn "$XMONAD/bin/brightness  0.1")

  -- Media control
  , ((0, xF86XK_AudioPlay), mediaAction PlayPause)
  , ((0, xF86XK_AudioNext), mediaAction MNext    )
  , ((0, xF86XK_AudioPrev), mediaAction MPrevious)
  , ((modm, xK_s), submap . M.fromList $ [
        ((0, xK_space), mediaAction PlayPause)
      , ((0, xK_n    ), mediaAction MNext    )
      , ((0, xK_p    ), mediaAction MPrevious)
      ])

  -- Layout
  , ((modm,               xK_space), sendMessage NextLayout)
  , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook xconf)
  , ((modm,               xK_slash), toggleGaps)
  , ((modm .|. shiftMask, xK_slash), toggleStatusBar)

  , ((modm, xK_comma ), sendMessage $ IncMasterN 1   )
  , ((modm, xK_period), sendMessage $ IncMasterN (-1))

    -- Focused window
  , ((modm .|. shiftMask, xK_c), kill <+> withFocused checkFocus)
  , ((modm, xK_h), sendMessage Shrink)
  , ((modm, xK_l), sendMessage Expand)
  , ((modm, xK_t), withFocused $ windows . W.sink)

  -- Focus and swap
  , ((modm,               xK_j), BW.focusDown  )
  , ((modm,               xK_k), BW.focusUp    )
  , ((modm,               xK_m), BW.focusMaster)
  , ((modm .|. shiftMask, xK_j), BW.swapDown   )
  , ((modm .|. shiftMask, xK_k), BW.swapUp     )
  -- don't care about swap to master

  -- Hidden (minimized) windows
  , ((modm, xK_u), chooseWindowToMaximize conf)
  , ((modm, xK_i), minimizeCurrentWindow)

  -- Misc
  , ((modm, xK_r), spawn "zsh -c 'sleep 0.2 && wiazac_client'")

  -- Scratchpads
  , ((modm, xK_f), namedScratchpadAction scratchpads "floaterm-min")
  , ((modm .|. shiftMask, xK_f), namedScratchpadAction scratchpads "floaterm")
  , ((modm, xK_d), namedScratchpadAction scratchpads "quicksearch")
  , ((modm, xK_e), namedScratchpadAction scratchpads "filexplorer")

  -- Workspaces
  , ((modm .|. shiftMask, xK_r), renameCurrentWorkspace XP.defaultConfig)
  , ((modm .|. shiftMask, xK_v), toggleCurrentWorkspaceVisibility)

  -- X server and xmonad control
  , ((modm, xK_x), submap . M.fromList $ [
        ((0, xK_r), spawn "$XMONAD/bin/xmonad/recompile -r")
      , ((0, xK_k), io exitSuccess)
      ])
  ]

  -- Workspaces movements
  ++
  [((m .|. modm, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces xconf) [xK_0 ..]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

  -- Swap workspaces
  ++
  [((modm .|. controlMask, k), windows $ swapWithCurrent i)
    | (i, k) <- zip (XMonad.workspaces xconf) [xK_0 ..]]

  ) <+> A.keybindings xconf

  where
    scratchpads = Scratchpads.scratchpads conf
