module QMonad.Lib.Xov (
  XovConf(..),
  xovOverlay,
) where

import XMonad hiding (borderWidth)
import qualified XMonad.StackSet as W
import XMonad.Prompt (mkUnmanagedWindow)
import Control.Concurrent (threadDelay)
import GHC.Float (int2Float)

data XovConf = XovConf {
  icon :: Maybe String,
  borderWidth :: Int,
  maxValue :: Int
}

mkOverlayWin :: Display -> Rectangle -> X Window
mkOverlayWin dpy rect = do
  rootw <- asks theRoot
  -- scrn <- gets $ W.screen . W.current . windowset
  liftIO $ mkUnmanagedWindow dpy (defaultScreenOfDisplay dpy) rootw
    (rect_x rect) (rect_y rect) (rect_width rect) (rect_height rect)

-- | Create a slider overlay.
xovOverlay :: XovConf -> Int -> Float -> X()
xovOverlay xvc val scl =
  withDisplay $ \dpy -> do
    let a = \a -> fromIntegral (2 * borderWidth xvc) + floor (a * scl)
    let bw = fromIntegral $ borderWidth xvc
        bounds = Rectangle 0 0 (a 100.0) (a 20.0)
        Rectangle _ _ w h = bounds

    win <- mkOverlayWin dpy bounds
    liftIO $
      mapWindow dpy win
      -- selectInput dpy win (keyPressMask .|. leaveWindowMask)

    gc <- liftIO $ createGC dpy win
    liftIO $ do
      Just green <- initColor dpy "green"
      Just red <- initColor dpy "red"
      Just white <- initColor dpy "white"

      -- border
      setForeground dpy gc white
      fillRectangle dpy win gc 0 0 w h

      -- progress
      setForeground dpy gc green
      let bsx = fromIntegral bw
          bsy = fromIntegral bw
          val' = int2Float val * int2Float (maxValue xvc) / 100.0
          h' = h - 2 * bw
          progress = floor $ val' * scl
          esx = fromIntegral $ bsx + fromIntegral progress
          empty = floor $ (100 - val') * scl
      fillRectangle dpy win gc bsx bsy progress h'
      setForeground dpy gc red
      fillRectangle dpy win gc esx bsy empty h'
    liftIO $ freeGC dpy gc

    liftIO $ sync dpy False
    io $ threadDelay 50000

    liftIO $ do
      unmapWindow dpy win
      destroyWindow dpy win
      sync dpy False
    return ()
