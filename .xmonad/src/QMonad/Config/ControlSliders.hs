module QMonad.Config.ControlSliders (
  volumeSlider,
) where

import XMonad

import QMonad.Lib.Sliders (Slider(..), SliderOverlay, makeSliderOverlay)

volumeSlider, brightnessSlider, opacitySlider :: Int -> X SliderOverlay

volumeSlider val = makeSliderOverlay $ slider "volume" val
brightnessSlider val = makeSliderOverlay $ slider "brightness" val
opacitySlider val = makeSliderOverlay $ slider "opacity" val

slider :: String -> Int -> Slider
slider id' val = Slider {
    value = val,
    identifier = id',
    customStyle = True,
    maxValue = Nothing
  }
