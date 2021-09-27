module QMonad.Lib.Xov (
  XovOverlay(..),
  XovConf(..),
  XovStyle(..),
  mkOverlay,
  destroyOverlay,
  drawOverlay,
) where

import XMonad hiding (borderWidth)
import XMonad.Prompt (mkUnmanagedWindow)
import GHC.Float (int2Float)
import Data.Maybe (isJust, fromMaybe)
import Control.Concurrent (threadDelay)

import Graphics.X11.Xft
import Graphics.X11.Xrender

import qualified XMonad.StackSet as W

data XovOverlay = XovOverlay {
  conf :: XovConf,
  style :: XovStyle,
  winScrn :: ScreenNumber,
  win :: Window
}

data XovConf = XovConf {
  icon :: Maybe String,
  iconWidth :: Int,
  showValue :: Bool,
  valueWidth :: Int,
  valuePrefix :: Maybe String,
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
  valueFont :: String,
  valueColor :: String,
  borderColor :: String,
  innerBorderColor :: String,
  progressColor :: String,
  emptyColor :: String,
  backgroundColor :: String
}

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

mkOverlayWin :: Display -> ScreenNumber -> Position -> Position -> Dimension -> Dimension -> IO Window
mkOverlayWin dpy scrn x y w h = do
  rootw <- rootWindow dpy scrn
  mkUnmanagedWindow dpy (screenOfDisplay dpy scrn) rootw
    x y w h

getBounds :: XovConf -> (Dimension ,Dimension)
getBounds conf = (w, h)
  where a = \a -> fi (2 * borderWidth conf) + a
        ibs = sum [fromEnum $ showValue conf, fromEnum . isJust $ icon conf]
        icn = if isJust $ icon conf then iconWidth conf else 0
        vl = if showValue conf then valueWidth conf else 0
        w = a $ width conf + fi (ibs * innerBorderWidth conf) + fi icn + fi vl
        h = a $ height conf

mkOverlay :: Display -> XovConf -> XovStyle -> Int -> IO XovOverlay
mkOverlay dpy conf style val = do
  let scrn = defaultScreen dpy
  let fw = displayWidth dpy scrn
      fh = displayHeight dpy scrn
  let (w,h) = getBounds conf
      x = fi $ (fw - fi w) `div` 2
      y = fi $ (fh - fi h) `div` 2
  win <- mkOverlayWin dpy scrn x y w h
  mapWindow dpy win
  -- selectInput dpy win (keyPressMask .|. leaveWindowMask)

  let overlay = XovOverlay {
    conf = conf,
    style = style,
    winScrn = scrn,
    win = win
  }
  drawOverlay dpy overlay val
  return overlay

destroyOverlay :: Display -> XovOverlay -> IO()
destroyOverlay dpy XovOverlay{win=win} = do
  unmapWindow dpy win
  destroyWindow dpy win
  sync dpy False

printCenteredString :: Display -> Screen -> Window -> XftFont -> Rectangle -> String -> String -> IO()
printCenteredString dpy scrn win font (Rectangle cx cy w h) fg s = do
  let colormap = defaultColormapOfScreen scrn
      visual = defaultVisualOfScreen scrn
  tw <- xglyphinfo_xOff <$> xftTextExtents dpy font s
  a <- fi <$> xftfont_ascent font
  let tx = fi (w `div` 2) - fi (tw `div` 2)
      ty = fi (h `div` 2) + fi (a `div` 2)
  withXftDraw dpy win visual colormap $
    \draw -> withXftColorName dpy visual colormap fg $
      \color -> xftDrawString draw color font (cx + tx) (cy + ty) s
  xftFontClose dpy font

drawOverlay :: Display -> XovOverlay -> Int -> IO()
drawOverlay dpy XovOverlay{conf=conf,style=style,winScrn=scrnNum,win=win} val = do
    let scrn = screenOfDisplay dpy scrnNum
    let bw = fi $ borderWidth conf
        iw = fi $ innerBorderWidth conf
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
    let y = fi bw
        h = fh - 2 * fi bw
    x <- case icon conf of
          Just icn -> do
            setForeground dpy gc bgc
            fillRectangle dpy win gc (fi bw) y (fi $ iconWidth conf) h
            setForeground dpy gc ibc
            fillRectangle dpy win gc (fi $ bw + iconWidth conf) y iw h

            font <- xftFontOpen dpy scrn (iconFont style)
            printCenteredString dpy scrn win font (Rectangle (fi bw) y (fi $ iconWidth conf) h)
              (iconColor style) icn

            return $ bw + iconWidth conf + fi iw
          Nothing -> return bw

    -- progress
    let boxw = width conf
    setForeground dpy gc bgc
    fillRectangle dpy win gc (fi x) y boxw h

    let bsx = fi $ x + padding conf
        bsy = fi $ fi y + padding conf
        boxw' = max 0 boxw - fi (2 * padding conf)
        h' = max 0 h - fi (2 * padding conf)
        val' = int2Float val * int2Float (maxValue conf) / 100.0
        progress = floor (val' * (int2Float . fi $ boxw') / 100.0)
        esx = fi $ bsx + fi progress
        empty = boxw' - progress
    setForeground dpy gc pc
    fillRectangle dpy win gc bsx bsy progress h'
    setForeground dpy gc ec
    fillRectangle dpy win gc esx bsy empty h'

    -- value indiactor
    let x' = x + fi boxw
    x'' <- if showValue conf then do
      setForeground dpy gc bgc
      fillRectangle dpy win gc (fi $ x' + fi iw) y (fi $ valueWidth conf) h
      setForeground dpy gc ibc
      fillRectangle dpy win gc (fi x') y iw h

      -- TODO: fix font thing
      font <- xftFontOpen dpy scrn (valueFont style)
      printCenteredString dpy scrn win font (Rectangle (fi x') y (fi $ valueWidth conf) h)
        (valueColor style) (show val ++ fromMaybe "" (valuePrefix conf))

      return $ x' + fi iw + valueWidth conf
    else
      return x'

    freeGC dpy gc
    sync dpy False
  where
    (fw,fh) = getBounds conf
