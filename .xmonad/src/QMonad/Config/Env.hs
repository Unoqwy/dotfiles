module QMonad.Config.Env (EnvConfig(..), loadEnvConfig) where
import GHC.IO
import System.Environment.Blank (getEnvDefault)

newtype EnvConfig = EnvConfig {
    terminal :: String
  }

loadEnvConfig :: IO EnvConfig
loadEnvConfig = do
  term <- getEnvDefault "XMONAD_TERMINAL" "kitty"
  return EnvConfig {
      terminal = term
    }
