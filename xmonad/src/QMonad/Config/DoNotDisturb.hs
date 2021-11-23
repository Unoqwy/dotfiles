module QMonad.Config.DoNotDisturb (
  setDND,
  isDND,
  toggleDND,
) where

import XMonad
import qualified XMonad.Util.ExtensibleState as XS

import Data.Char (toLower)
import QMonad.Config.Util (awaitSpawn)

newtype DND = DND Bool
  deriving (Read, Show, Typeable)

instance ExtensionClass DND where
  initialValue = DND False
  extensionType = PersistentExtension

isDND :: X Bool
isDND = do
  DND state <- XS.get
  return state

setDND :: Bool -> X()
setDND state = do
  XS.put $ DND state
  awaitSpawn $ "dunstctl set-paused " ++ (map toLower . show $ state)

toggleDND :: X()
toggleDND = do
  DND state <- XS.get
  setDND $ not state
