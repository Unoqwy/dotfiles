module QMonad.Lib.Window.Opacity (
  setWindowOpacity,
) where

import XMonad
import Foreign.C.Types (CLong(..))
import Data.Word (Word32)

setWindowOpacity :: Window -> Float -> X()
setWindowOpacity win opacity = do
  let opacity' = floor $ fromIntegral (maxBound :: Word32) * opacity
  withDisplay $ \dpy -> do
    atom <- getAtom "_NET_WM_WINDOW_OPACITY"
    io $ changeProperty32 dpy win atom cARDINAL propModeReplace [fromIntegral opacity']
