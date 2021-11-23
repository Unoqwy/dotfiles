module QMonad.Config.Util (
  doRectFloatSeventy,
  doRectFloatEighty,

  awaitSpawn,
  runSH,
  runSHGetExitCode,
) where

import XMonad
import XMonad.Hooks.ManageHelpers (doRectFloat)
import XMonad.Util.Run (runProcessWithInput)
import qualified XMonad.StackSet as W

import Data.Char (isSpace)
import Control.Monad (void)
import System.IO
import System.Process (runInteractiveCommand, waitForProcess)
import System.Exit (ExitCode (ExitSuccess))

doRectFloatSeventy, doRectFloatEighty :: ManageHook
doRectFloatSeventy = doRectFloat $ W.RationalRect 0.15 0.15 0.7 0.7
doRectFloatEighty = doRectFloat $ W.RationalRect 0.1 0.1 0.8 0.8

-- Shell
awaitSpawn :: String -> X()
awaitSpawn = void . runSH

runSH :: MonadIO m => String -> m String
runSH command = do
  out <- runProcessWithInput "/bin/sh" ["-c", command] []
  return $ (reverse . dropWhile isSpace . reverse) out

runSHGetExitCode :: MonadIO m => String -> m ExitCode
runSHGetExitCode cmd = io $ do
  uninstallSignalHandlers
  (hIn, hOut, hErr, p) <- runInteractiveCommand cmd
  mapM_ hClose [hIn, hErr]
  waitForProcess p <* hGetContents hOut <* installSignalHandlers
