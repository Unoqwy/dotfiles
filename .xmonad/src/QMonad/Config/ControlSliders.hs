module QMonad.Config.ControlSliders (
  opacityControlSlider
) where

import XMonad
import QMonad.Config.Env
import QMonad.Lib.Sliders
import qualified XMonad.Util.ExtensibleState as XS

opacityControlSlider :: X()
opacityControlSlider = do
  opac <- getOpacity
  mkSlider sId opac

getOpacity :: X Int
getOpacity = do
  default_opacity <$> XS.gets envConfig
