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
        icon = Just "\xf58f",
        iconWidth = 40,
        showValue = True,
        valueWidth = 40,
        valuePrefix = Just "%",
        width = 400,
        height = 40,
        borderWidth = 2,
        innerBorderWidth = 2,
        padding = 3,
        maxValue = 100
      }
  let style = XovStyle {
        iconFont = "Font Awesome 5 Pro:size=16:style=Solid",
        iconColor = "#a3a1a4",
        valueFont = "Jetbrains Mono:size=14",
        valueColor = "#a3a1a4",
        borderColor = "#4c516d",
        innerBorderColor = "#4c516d",
        progressColor = "#E1BEE7",
        emptyColor = "#242530",
        backgroundColor = "#242530"
      }
  overlay <- mkOverlay dpy conf style 100
  drawOverlay dpy overlay 35
  threadDelay 250000
  destroyOverlay dpy overlay