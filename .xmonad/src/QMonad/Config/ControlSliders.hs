module QMonad.Config.ControlSliders (
  mkSlider
) where

import XMonad hiding (borderWidth)
import Control.Monad (void)
import Control.Concurrent (threadDelay)
import GHC.Conc (forkIO)
import QMonad.Lib.Xov

mkSlider :: X()
mkSlider = withDisplay $ \dpy -> void . liftIO $ forkIO (initSlider dpy)

initSlider :: Display -> IO()
initSlider dpy = do
  let conf = XovConf {
        width = 250,
        height = 40,
        icon = Nothing,
        borderWidth = 2,
        maxValue = 100
      }
  Just blue <- initColor dpy "green"
  Just red <- initColor dpy "gray"
  Just gray <- initColor dpy "blue"
  let style = XovStyle {
        borderColor = gray,
        progressColor = blue,
        emptyColor = red
      }
  overlay <- mkOverlay dpy conf style 100
  mapM_ (update dpy overlay) [1..100]
  destroyOverlay dpy overlay

update dpy overlay val = do
  drawOverlay dpy overlay val
  threadDelay 50000
