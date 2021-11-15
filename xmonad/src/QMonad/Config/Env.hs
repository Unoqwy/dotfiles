module QMonad.Config.Env (EnvConfig(..), EnvState(..), loadEnvConfig, localBin) where

import XMonad (Typeable, ExtensionClass(..))
import GHC.IO
import System.Environment.Blank (getEnvDefault)

import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Either.Combinators (rightToMaybe)
import Data.Yaml (decodeFileEither)

import QMonad.Config.Theme

data EnvState = EnvState {
    envConfig :: EnvConfig,
    globalOpacity :: Int,
    colorTemp :: Int
  } deriving Typeable

instance ExtensionClass EnvState where
  initialValue = EnvState {
      envConfig = EnvConfig {
          xmonad_path = ""
        , terminal = ""
        , default_opacity = 100
        , theme = defaultTheme'
        },
      globalOpacity = 100,
      colorTemp = 6500
    }

data EnvConfig = EnvConfig {
    xmonad_path :: String,
    terminal :: String,
    default_opacity :: Int,
    theme :: Theme
  } deriving Typeable

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

localBin :: EnvConfig -> String -> String
localBin EnvConfig{xmonad_path = root} bin = root ++ "/bin/" ++ bin
