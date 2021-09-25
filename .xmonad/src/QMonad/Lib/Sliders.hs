module QMonad.Lib.Sliders (
    Slider(..),
    SliderOverlay(..),
    makeSliderOverlay,
    displayOverlayValue,
    sliderIncrease,
    sliderDecrease,
) where

import XMonad
import XMonad.Util.Run (spawnPipe)

import GHC.IO.Handle (Handle)
import System.IO (hPrint)

maybeParam :: Maybe a -> (a -> String) -> String -> String
maybeParam (Just v) s param = " " ++ param ++ " " ++ s v
maybeParam _ _ _ = ""

openXobHandle :: Maybe String -> Maybe Int -> X Handle
openXobHandle s m = do
    spawnPipe $ "xob" ++ maybeParam s show "-s" ++ maybeParam m show "-m"

data Slider = Slider {
    value :: Int,

    identifier :: String,
    customStyle :: Bool,
    maxValue :: Maybe Int
  }

data SliderOverlay = SliderOverlay {
    slider :: Slider,
    xobStdin :: Handle
  }

makeSliderOverlay :: Slider -> X SliderOverlay
makeSliderOverlay slider@Slider{value=val, identifier=id'} = do
    xobHandle <- openXobHandle (if customStyle slider then Just id' else Nothing) (maxValue slider)
    return SliderOverlay {
        slider = slider,
        xobStdin = xobHandle
      }

displayOverlayValue :: SliderOverlay -> X()
displayOverlayValue o@SliderOverlay{xobStdin=hdl} = do
    liftIO $ hPrint hdl (value . slider $ o)

sliderIncrease :: SliderOverlay -> Int -> X SliderOverlay
sliderIncrease o inc = do
    let val = (value . slider $ o) + inc
    let slider' = (slider o) { value = val }
    let o' = o { slider = slider' }
    displayOverlayValue o'
    return o'

sliderDecrease :: SliderOverlay -> Int -> X SliderOverlay
sliderDecrease o dec = sliderIncrease o (-dec)
