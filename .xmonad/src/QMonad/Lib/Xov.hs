module QMonad.Lib.Xov (
  XovOverlay(..),
  XovConf(..),
  XovStyle(..),
  XovHAnchor(..),
  XovVAnchor(..),
  mkOverlay,
  destroyOverlay,
  drawOverlay,
  updateProgress,
) where

import Prelude hiding (Left, Right)
import XMonad hiding (borderWidth)
import Control.Monad (when)
import XMonad.Prompt (mkUnmanagedWindow)
import GHC.Float (int2Float)
import Data.Maybe (isJust, fromMaybe)
import Data.Word (Word32)
import Control.Concurrent (threadDelay)

import Graphics.X11.Xft
import Graphics.X11.Xrender
import Graphics.X11.Xinerama (getScreenInfo)

import qualified XMonad.StackSet as W

data XovOverlay = XovOverlay {
  conf :: XovConf,
  style :: XovStyle,
  winScrn :: ScreenNumber,
  win :: Window,
  dimWin :: Maybe Window
}

data XovConf = XovConf {
  icon :: Maybe String,
  iconWidth :: Int,
  showValue :: Bool,
  valueWidth :: Int,
  valueSuffix :: Maybe String,
  width :: Dimension,
  height :: Dimension,
  padding :: Int,
  borderWidth :: Int,
  innerBorderWidth :: Int,
  minValue :: Int,
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

getBounds :: XovConf -> (Dimension ,Dimension)
getBounds conf = (w, h)
  where a = \a -> fi (2 * borderWidth conf) + a
        ibs = sum [fromEnum $ showValue conf, fromEnum . isJust $ icon conf]
        icn = if isJust $ icon conf then iconWidth conf else 0
        vl = if showValue conf then valueWidth conf else 0
        w = a $ width conf + fi (ibs * innerBorderWidth conf) + fi icn + fi vl
        h = a $ height conf

data XovHAnchor = HCenter | Left Int | Right Int
data XovVAnchor = VCenter | Bottom Int | Top Int

mkOverlay :: Display -> ScreenId -> XovHAnchor -> XovVAnchor -> XovConf -> XovStyle -> Int -> IO XovOverlay
mkOverlay dpy (S sid) hac vac conf style val = do
  let scrn = defaultScreen dpy
  sid <- (!! sid) <$> getScreenInfo dpy
  let Rectangle sx sy fw fh = sid
  let (w,h) = getBounds conf
      x = case hac of
          HCenter -> (fw - fi w) `div` 2
          Left a -> fi a
          Right a -> fw - fi a
      y = case vac of
          VCenter -> (fh - fi h) `div` 2
          Top a -> fi a
          Bottom a -> fh - fi a - fi h

  rootw <- rootWindow dpy scrn
  dimWin <- mkUnmanagedWindow dpy (screenOfDisplay dpy scrn) rootw sx sy fw fh
  win <- mkUnmanagedWindow dpy (screenOfDisplay dpy scrn) rootw (sx + fi x) (sy + fi y) w h
  let overlay = XovOverlay {
    conf = conf,
    style = style,
    winScrn = scrn,
    win = win,
    dimWin = Just dimWin
  }

  mapWindow dpy dimWin
  drawDim dpy overlay fw fh 30.0
  mapWindow dpy win
  drawOverlay dpy overlay val
  return overlay

destroyOverlay :: Display -> XovOverlay -> IO()
destroyOverlay dpy XovOverlay{win=win,dimWin=dimWin} = do
  destroyWindow' dpy dimWin
  destroyWindow' dpy (Just win)
  sync dpy False

destroyWindow' :: Display -> Maybe Window -> IO()
destroyWindow' dpy (Just win) = do
  unmapWindow dpy win
  destroyWindow dpy win
destroyWindow' dpy Nothing = return ()

printCenteredString :: Display -> Screen -> Window -> XftFont -> Rectangle -> String -> String -> IO()
printCenteredString dpy scrn win font (Rectangle cx cy w h) fg s = do
  let colormap = defaultColormapOfScreen scrn
      visual = defaultVisualOfScreen scrn
  tw <- xglyphinfo_xOff <$> xftTextExtents dpy font s
  a <- fi <$> xftfont_ascent font
  b <- fi <$> xftfont_descent font
  let tx = fi (w `div` 2) - fi (tw `div` 2)
      ty = fi (h `div` 2) + fi (a `div` 2) - fi (b `div` 2)
  withXftDraw dpy win visual colormap $
    \draw -> withXftColorName dpy visual colormap fg $
      \color -> xftDrawString draw color font (cx + tx) (cy + ty) s
  xftFontClose dpy font

setOpacity :: Display -> Window -> Float -> IO()
setOpacity dpy win opacity = do
  let opacity' = fi . floor . (/ 100) $ fi (maxBound :: Word32) * opacity
  atom <- internAtom dpy "_NET_WM_WINDOW_OPACITY" False
  changeProperty32 dpy win atom cARDINAL propModeReplace [opacity']

drawDim :: Display -> XovOverlay -> Dimension -> Dimension -> Float -> IO()
drawDim dpy XovOverlay{dimWin=dimWin} fw fh opacity = do
    let Just win = dimWin
    gc <- createGC dpy win

    Just black <- initColor dpy "#000000"
    setForeground dpy gc black
    fillRectangle dpy win gc 0 0 fw fh
    setOpacity dpy win opacity

    freeGC dpy gc
    sync dpy False

data XovMarks = Mks (Maybe Int) Int (Maybe Int)

mks :: XovConf -> XovMarks
mks conf = Mks icn prg vl
  where bw = fi $ borderWidth conf
        iw = fi $ innerBorderWidth conf
        (icn,prg) = if isJust $ icon conf then (Just bw, bw + iconWidth conf + iw) else (Nothing, bw)
        vl = if showValue conf then Just $ prg + fi (width conf) else Nothing

drawOverlay :: Display -> XovOverlay -> Int -> IO()
drawOverlay dpy o@XovOverlay{conf=conf,style=style,winScrn=scrnNum,win=win} val = do
    let scrn = screenOfDisplay dpy scrnNum
    let bw = fi $ borderWidth conf
        iw = fi $ innerBorderWidth conf
    gc <- createGC dpy win

    Just bc <- initColor dpy (borderColor style)
    Just ibc <- initColor dpy (innerBorderColor style)
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

    drawProgress dpy gc o val
    when (showValue conf) (drawValue dpy gc o val)

    freeGC dpy gc
    sync dpy False
  where
    (fw,fh) = getBounds conf

drawProgress :: Display -> GC -> XovOverlay -> Int -> IO()
drawProgress dpy gc XovOverlay{conf=conf,style=style,winScrn=scrnNum,win=win} val = do
    Just pc <- initColor dpy (progressColor style)
    Just ec <- initColor dpy (emptyColor style)
    Just bgc <- initColor dpy (backgroundColor style)

    let boxw = width conf
    setForeground dpy gc bgc
    fillRectangle dpy win gc (fi x) y boxw h

    let bsx = fi $ x + padding conf
        bsy = fi $ fi y + padding conf
        boxw' = max 0 boxw - fi (2 * padding conf)
        h' = max 0 h - fi (2 * padding conf)
        over = maxValue conf - minValue conf
        val' = int2Float (val - minValue conf) * 100.0 / int2Float over
        progress = floor (val' * (int2Float . fi $ boxw') / 100.0)
        esx = fi $ bsx + fi progress
        empty = boxw' - progress
    setForeground dpy gc pc
    fillRectangle dpy win gc bsx bsy progress h'
    setForeground dpy gc ec
    fillRectangle dpy win gc esx bsy empty h'
  where
    scrn = screenOfDisplay dpy scrnNum
    (fw,fh) = getBounds conf
    h = fh - 2 * fi bw
    bw = fi $ borderWidth conf
    Mks _ x _ = mks conf
    y = fi bw

drawValue :: Display -> GC -> XovOverlay -> Int -> IO()
drawValue dpy gc XovOverlay{conf=conf,style=style,winScrn=scrnNum,win=win} val = do
    Just ibc <- initColor dpy (innerBorderColor style)
    Just bgc <- initColor dpy (backgroundColor style)

    setForeground dpy gc bgc
    fillRectangle dpy win gc (fi x + fi iw) y (fi $ valueWidth conf) h
    setForeground dpy gc ibc
    fillRectangle dpy win gc (fi x) y iw h

    font <- xftFontOpen dpy scrn (valueFont style)
    printCenteredString dpy scrn win font (Rectangle (fi x) y (fi $ valueWidth conf) h)
      (valueColor style) (show val ++ fromMaybe "" (valueSuffix conf))
  where
    scrn = screenOfDisplay dpy scrnNum
    (fw,fh) = getBounds conf
    h = fh - 2 * fi bw
    bw = fi $ borderWidth conf
    iw = fi $ innerBorderWidth conf
    Mks _ _ (Just x) = mks conf
    y = fi bw

updateProgress :: Display -> XovOverlay -> Int -> IO()
updateProgress dpy o val = do
  gc <- createGC dpy (win o)
  drawProgress dpy gc o val
  drawValue dpy gc o val
  freeGC dpy gc
  sync dpy False
