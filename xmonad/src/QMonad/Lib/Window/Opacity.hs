module QMonad.Lib.Window.Opacity (
  setWindowOpacity,
) where

import XMonad
import Foreign.C.Types (CLong(..))
import Data.Word (Word32)

setWindowOpacity :: Window -> Float -> X()
setWindowOpacity win opacity = do
  let opacity'  = max 0 (min opacity 1)
  let opacity'' = floor $ fromIntegral (maxBound :: Word32) * opacity
  withDisplay $ \dpy -> do
    atom <- getAtom "_NET_WM_WINDOW_OPACITY"
    let cardinal = min (fromIntegral opacity'') 4294967295
    io $ changeProperty32 dpy win atom cARDINAL propModeReplace [cardinal]
