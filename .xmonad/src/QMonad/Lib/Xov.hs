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
import Data.Maybe (isJust)

data XovOverlay = XovOverlay {
  conf :: XovConf,
  style :: XovStyle,
  win :: Window
}

data XovConf = XovConf {
  icon :: Maybe String,
  iconWidth :: Int,
  showValue :: Bool,
  valueWidth :: Int,
  width :: Dimension,
  height :: Dimension,
  padding :: Int,
  borderWidth :: Int,
  innerBorderWidth :: Int,
  maxValue :: Int
}

data XovStyle = XovStyle {
  iconFont :: String,
  iconColor :: String,
  iconSize :: Int,
  borderColor :: String,
  innerBorderColor :: String,
  progressColor :: String,
  emptyColor :: String,
  backgroundColor :: String
}

mkOverlayWin :: Display -> ScreenNumber -> Position -> Position -> Dimension -> Dimension -> IO Window
mkOverlayWin dpy scrn x y w h = do
  rootw <- rootWindow dpy scrn
  mkUnmanagedWindow dpy (screenOfDisplay dpy scrn) rootw
    x y w h

getBounds :: XovConf -> (Dimension ,Dimension)
getBounds conf = (w, h)
  where a = \a -> fromIntegral (2 * borderWidth conf) + a
        ibs = sum [fromEnum $ showValue conf, fromEnum . isJust $ icon conf]
        icn = if isJust $ icon conf then iconWidth conf else 0
        vl = if showValue conf then valueWidth conf else 0
        w = a $ width conf + fromIntegral (ibs * innerBorderWidth conf) + fromIntegral icn + fromIntegral vl
        h = a $ height conf

mkOverlay :: Display -> XovConf -> XovStyle -> Int -> IO XovOverlay
mkOverlay dpy conf style val = do
  let scrn = defaultScreen dpy
  let fw = displayWidth dpy scrn
      fh = displayHeight dpy scrn
  let (w,h) = getBounds conf
      x = fromIntegral $ (fw - fromIntegral w) `div` 2
      y = fromIntegral $ (fh - fromIntegral h) `div` 2
  win <- mkOverlayWin dpy scrn x y w h
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
        iw = fromIntegral $ innerBorderWidth conf
    gc <- createGC dpy win

    Just bc <- initColor dpy (borderColor style)
    Just ibc <- initColor dpy (innerBorderColor style)
    Just pc <- initColor dpy (progressColor style)
    Just ec <- initColor dpy (emptyColor style)
    Just bgc <- initColor dpy (backgroundColor style)

    -- outline
    setForeground dpy gc bc
    fillRectangle dpy win gc 0 0 fw fh

    -- icon
    let y = fromIntegral bw
        h = fh - 2 * fromIntegral bw
    x <- case icon conf of
          Just _ -> do
            setForeground dpy gc bgc
            fillRectangle dpy win gc (fromIntegral bw) y (fromIntegral $ iconWidth conf) h
            setForeground dpy gc ibc
            fillRectangle dpy win gc (fromIntegral $ bw + iconWidth conf) y iw h
            return $ bw + iconWidth conf + fromIntegral iw
          Nothing -> return bw

    -- progress
    let boxw = width conf
    setForeground dpy gc bgc
    fillRectangle dpy win gc (fromIntegral x) y boxw h

    let bsx = fromIntegral $ x + padding conf
        bsy = fromIntegral $ fromIntegral y + padding conf
        boxw' = max 0 boxw - fromIntegral (2 * padding conf)
        h' = max 0 h - fromIntegral (2 * padding conf)
        val' = int2Float val * int2Float (maxValue conf) / 100.0
        progress = floor (val' * (int2Float . fromIntegral $ boxw') / 100.0)
        esx = fromIntegral $ bsx + fromIntegral progress
        empty = boxw' - progress
    setForeground dpy gc pc
    fillRectangle dpy win gc bsx bsy progress h'
    setForeground dpy gc ec
    fillRectangle dpy win gc esx bsy empty h'

    -- value indiactor
    let x' = x + fromIntegral boxw
    setForeground dpy gc bgc
    fillRectangle dpy win gc (fromIntegral $ x' + fromIntegral iw) y (fromIntegral $ valueWidth conf) h
    setForeground dpy gc ibc
    fillRectangle dpy win gc (fromIntegral x') y iw h

    freeGC dpy gc
    sync dpy False
  where
    (fw,fh) = getBounds conf
