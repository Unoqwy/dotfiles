module QMonad.Config.Env (
  EnvConfig(..),
  EnvState(..),
  EnvState'(..),
  withState,
  updateState,
  putStateOrUpdate,
  getState,

  loadEnvConfig,
  saveEnvState,

  localBin,
  localBin',
) where

import XMonad (X, catchIO, whenJust, Typeable, ExtensionClass(..), StateExtension(..), updateLayout)
import qualified XMonad.Util.ExtensibleState as XS

import Control.Monad (when)
import GHC.IO
import System.Environment.Blank (getEnvDefault)

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe, isNothing)
import Data.Either.Combinators (rightToMaybe)
import Data.Yaml (decodeFileEither)

import QMonad.Config.Theme

data EnvState = EnvState {
    envConfig :: EnvConfig,
    globalOpacity :: Int,
    colorTemp :: Int
  } deriving (Typeable, Read, Show)

newtype EnvState' = ES { envState :: Maybe EnvState }
    deriving (Typeable, Read, Show)

instance ExtensionClass EnvState' where
  initialValue = ES Nothing
  extensionType = PersistentExtension

data EnvConfig = EnvConfig {
    xmonad_path :: String,
    terminal :: String,
    default_opacity :: Int,
    theme :: Theme
  } deriving (Typeable, Read, Show)

withState :: (EnvState -> X()) -> X()
withState action = flip whenJust action =<< XS.gets envState

updateState :: (EnvState -> EnvState) -> X()
updateState update = flip whenJust (XS.put . ES . Just . update) =<< XS.gets envState

putStateOrUpdate :: EnvState -> (EnvState -> EnvState) -> X()
putStateOrUpdate put update = do
  state <- XS.gets envState
  when (isNothing state) (XS.put . ES . Just $ put)
  updateState update

getState :: (EnvState -> a) -> a -> X a
getState get def = do
  state <- XS.gets envState
  return $ case state of
    Just es -> get es
    _ -> def

loadEnvConfig :: IO EnvConfig
loadEnvConfig = do
  xmonad_path' <- getEnvDefault "XMONAD" "~/.xmonad"
  terminal' <- getEnvDefault "XMONAD_TERMINAL" "kitty"
  default_opacity' <- getEnvDefault "XMONAD_DEFAULT_OPACITY" "98"

  themeFile <- getEnvDefault "XMONAD_THEME_FILE" (xmonad_path' ++ "/config/theme.yml")
  theme' <- fromMaybe defaultTheme' . rightToMaybe <$> decodeFileEither themeFile

  return EnvConfig {
      xmonad_path = xmonad_path',
      terminal = terminal',
      default_opacity = fromMaybe 100 (readMaybe default_opacity'),
      theme = theme'
    }

saveEnvState :: X()
saveEnvState = withState $ \es -> do
  let xmonad_path' = xmonad_path . envConfig $ es
  let opacity' = globalOpacity es
  let vars = [ ("XMONAD_DEFAULT_OPACITY", opacity') ]
  let contents = concatMap (\(k,v) -> "export " ++ k ++ "=" ++ show v ++ "\n") vars
  catchIO $ writeFile (xmonad_path' ++ "/.env-state") contents

localBin :: EnvConfig -> String -> String
localBin EnvConfig{xmonad_path = root} bin = root ++ "/bin/" ++ bin

localBin' :: String -> X String
localBin' bin = (++ "/bin/" ++ bin) <$> getState (xmonad_path . envConfig) "~/.xmonad"
