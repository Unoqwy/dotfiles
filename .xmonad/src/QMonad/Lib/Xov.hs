module QMonad.Lib.Xov (
  XovOverlay(..),
  XovConf(..),
  XovStyle(..),
  mkOverlay,
  destroyOverlay,
  drawOverlay,
) where

import XMonad hiding (borderWidth)
import qualified XMonad.StackSet as W
import XMonad.Prompt (mkUnmanagedWindow)
import Control.Concurrent (threadDelay)
import GHC.Float (int2Float)

data XovOverlay = XovOverlay {
  conf :: XovConf,
  style :: XovStyle,

  win :: Window
  -- width :: Dimension,
  -- height :: Dimension
}

data XovConf = XovConf {
  icon :: Maybe String,
  width :: Dimension,
  height :: Dimension,
  borderWidth :: Int,
  maxValue :: Int
}

data XovStyle = XovStyle {
  borderColor :: Pixel,
  progressColor :: Pixel,
  emptyColor :: Pixel
}

mkOverlayWin :: Display -> Rectangle -> IO Window
mkOverlayWin dpy rect = do
  -- scrn <- gets $ W.screen . W.current . windowset
  let scrn = defaultScreen dpy
  rootw <- rootWindow dpy scrn
  mkUnmanagedWindow dpy (screenOfDisplay dpy scrn) rootw
    (rect_x rect) (rect_y rect) (rect_width rect) (rect_height rect)

getBounds :: XovConf -> Rectangle
getBounds conf = Rectangle 0 0 (a $ width conf) (a $ height conf)
  where a = \a -> fromIntegral (2 * borderWidth conf) + a

mkOverlay :: Display -> XovConf -> XovStyle -> Int -> IO XovOverlay
mkOverlay dpy conf style val = do
  win <- mkOverlayWin dpy (getBounds conf)
  mapWindow dpy win
  -- selectInput dpy win (keyPressMask .|. leaveWindowMask)

  let overlay = XovOverlay {
    conf = conf,
    style = style,
    win = win
  }
  drawOverlay dpy overlay val
  return overlay

destroyOverlay :: Display -> XovOverlay -> IO()
destroyOverlay dpy XovOverlay{win=win} = do
  unmapWindow dpy win
  destroyWindow dpy win
  sync dpy False

drawOverlay :: Display -> XovOverlay -> Int -> IO()
drawOverlay dpy XovOverlay{conf=conf,style=style,win=win} val = do
    let bw = fromIntegral $ borderWidth conf
    let boxw = width conf
    gc <- createGC dpy win

    -- outline
    setForeground dpy gc (borderColor style)
    fillRectangle dpy win gc 0 0 fw fh

    -- progress
    let bsx = fromIntegral bw
        bsy = fromIntegral bw
        val' = int2Float val * int2Float (maxValue conf) / 100.0
        h' = fh - 2 * bw
        progress = floor (val' * (int2Float . fromIntegral $ boxw) / 100.0)
        esx = fromIntegral $ bsx + fromIntegral progress
        empty = boxw - progress
    setForeground dpy gc (progressColor style)
    fillRectangle dpy win gc bsx bsy progress h'
    setForeground dpy gc (emptyColor style)
    fillRectangle dpy win gc esx bsy empty h'

    freeGC dpy gc
    sync dpy False
  where
    Rectangle _ _ fw fh = getBounds conf
