module QMonad.Config.Keybindings (
    keybindings
) where

import XMonad
import System.Exit (exitSuccess)
import qualified Data.Map        as M
import qualified XMonad.StackSet as W

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

import qualified QMonad.Config.Applications as A
import qualified QMonad.Config.Hooks as Hooks
import qualified QMonad.Config.Prompt as XP

-- Media controls
data MediaControl = PlayPause | MNext | MPrevious

instance Show MediaControl where
  show PlayPause = "PlayPause"
  show MNext = "Next"
  show MPrevious = "Previous"

mprisCtl :: String -> MediaControl -> X()
mprisCtl n c = spawn
  $ "dbus-send --print-reply --dest=org.mpris.MediaPlayer2."  ++ show n
  ++ " /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player." ++ show c

spotifyCtl :: MediaControl -> X()
spotifyCtl mediaCtl = do
  mprisCtl "spotify" mediaCtl
  mprisCtl "spotifyd" mediaCtl

-- Toggles
toggleGaps :: X()
toggleGaps = do
  toggleScreenSpacingEnabled
  toggleWindowSpacingEnabled

-- Keybindings
keybindings conf@XConfig {XMonad.modMask = modm} = M.fromList ([
  -- Brightness control
    ((modm, xK_Left ), spawn "$XMONAD/bin/brightness -0.1")
  , ((modm, xK_Right), spawn "$XMONAD/bin/brightness  0.1")

  -- Media control
  , ((0, xF86XK_AudioPlay), spotifyCtl PlayPause)
  , ((0, xF86XK_AudioNext), spotifyCtl MNext    )
  , ((0, xF86XK_AudioPrev), spotifyCtl MPrevious)
  , ((modm, xK_s), submap . M.fromList $ [
        ((0, xK_space), spotifyCtl PlayPause)
      , ((0, xK_n    ), spotifyCtl MNext    )
      , ((0, xK_p    ), spotifyCtl MPrevious)
      ])

  -- Layout
  , ((modm,               xK_space), sendMessage NextLayout)
  , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)
  , ((modm,               xK_slash), toggleGaps)

  , ((modm, xK_comma ), sendMessage $ IncMasterN 1   )
  , ((modm, xK_period), sendMessage $ IncMasterN (-1))

    -- Focused window
  , ((modm .|. shiftMask, xK_c), kill)
  , ((modm, xK_h), sendMessage Shrink)
  , ((modm, xK_l), sendMessage Expand)
  , ((modm, xK_t), withFocused $ windows . W.sink)

  -- Focus and swap
  , ((modm,               xK_j), windows W.focusDown  )
  , ((modm,               xK_k), windows W.focusUp    )
  , ((modm,               xK_m), windows W.focusMaster)
  , ((modm .|. shiftMask, xK_j), windows W.swapDown   )
  , ((modm .|. shiftMask, xK_k), windows W.swapUp     )
  , ((modm .|. shiftMask, xK_m), windows W.swapMaster )

  -- Scratchpads
  , ((modm, xK_f), namedScratchpadAction Hooks.scratchpads "floaterm")
  , ((modm, xK_d), namedScratchpadAction Hooks.scratchpads "quicksearch")

  -- Workspaces
  , ((modm .|. shiftMask, xK_r), renameCurrentWorkspace XP.defaultConfig)
  , ((modm .|. shiftMask, xK_v), toggleCurrentWorkspaceVisibility)

  -- X server and xmonad control
  , ((modm, xK_x), submap . M.fromList $ [
        ((0, xK_r), spawn "qmonad --recompile; qmonad --restart")
      , ((0, xK_k), io exitSuccess)
      ])
  ]

  -- Workspaces movements
  ++
  [((m .|. modm, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_0 ..]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

  -- Swap workspaces
  ++
  [((modm .|. controlMask, k), windows $ swapWithCurrent i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_0 ..]]

  ) <+> A.keybindings conf

