module QMonad.Config.Env (EnvConfig(..), EnvConfig'(..), loadEnvConfig, localBin) where

import XMonad (Typeable, ExtensionClass(..))
import System.Environment.Blank (getEnvDefault)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import GHC.IO

newtype EnvConfig' = EnvConfig' {
    envConfig :: EnvConfig
  } deriving Typeable

instance ExtensionClass EnvConfig' where
  initialValue = EnvConfig' {
      envConfig = EnvConfig {
          xmonad_path = ""
        , terminal = ""
        , default_opacity = 100
        }
    }

data EnvConfig = EnvConfig {
    xmonad_path :: String,
    terminal :: String,
    default_opacity :: Int
  } deriving Typeable

loadEnvConfig :: IO EnvConfig
loadEnvConfig = do
  xmonad_path' <- getEnvDefault "XMONAD" "~/.xmonad"
  terminal' <- getEnvDefault "XMONAD_TERMINAL" "kitty"
  default_opacity' <- getEnvDefault "XMONAD_DEFAULT_OPACITY" "98"
  return EnvConfig {
      xmonad_path = xmonad_path',
      terminal = terminal',
      default_opacity = fromMaybe 100 (readMaybe default_opacity')
    }

localBin :: EnvConfig -> String -> String
localBin EnvConfig{xmonad_path = root} bin = root ++ "/bin/" ++ bin
