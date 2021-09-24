module QMonad.Lib.Window.Minimize (
    minimizedStack,
    minimizeWindow,
    maximizeWindow,
    sortMinimizedWindows,
) where

import XMonad
import XMonad.Util.Minimize (Minimized(minimizedStack))

import Foreign.C.Types (CLong(..))
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (sortBy)

import qualified XMonad.Actions.Minimize as X.A.Minimize (minimizeWindow, maximizeWindowAndFocus)

import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

sortTimestamp a b
  | snd a > snd b = LT
  | snd a < snd b = GT
  | otherwise = EQ

sortMinimizedWindows :: X [Window]
sortMinimizedWindows = do
  minimized <- XS.gets minimizedStack
  wset <- gets windowset

  let minimized' = filter (`elem` minimized) (W.integrate' $ W.stack (W.workspace $ W.current wset))
  withTime <- mapM (\w -> do
      time <- getMinimizeTime w
      return (w, time)
    ) minimized'
  let sorted = sortBy sortTimestamp withTime
  return $ map fst (sortBy sortTimestamp sorted)

getMinimizeTime :: Window -> X CLong
getMinimizeTime win =
  withDisplay $ \dpy -> do
    atom <- getAtom "MINIMIZED_AT"
    prop <- liftIO $ getWindowProperty32 dpy atom win
    return $ case prop of
          Just [a] -> a
          _ -> 0

minimizeWindow :: Window -> X()
minimizeWindow win = do
  X.A.Minimize.minimizeWindow win
  withDisplay $ \dpy -> do
    atom <- getAtom "MINIMIZED_AT"
    io $ do
      timestamp <- round <$> getPOSIXTime
      changeProperty32 dpy win atom cARDINAL propModeReplace [CLong timestamp]

maximizeWindow :: Window -> X()
maximizeWindow win = do
  X.A.Minimize.maximizeWindowAndFocus win
  withDisplay $ \dpy -> do
    atom <- getAtom "MINIMIZED_AT"
    io $ deleteProperty dpy win atom
