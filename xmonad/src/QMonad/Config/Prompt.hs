module QMonad.Config.Prompt (
  defaultConfig
) where

import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)

import QMonad.Config.Env (EnvConfig(..))
import qualified QMonad.Config.Theme as T

defaultConfig :: EnvConfig -> XPConfig
defaultConfig conf = def {
    font = "xft:" ++ T.font t' ++ ":size=" ++ show (T.fontSize t')
              ++ ":antialias=true:hinting=true"
  , fgColor = T.foreground t'
  , bgColor = T.background t'
  , fgHLight = T.selectedForeground t'
  , bgHLight = T.selectedBackground t'

  , promptBorderWidth = fromIntegral $ T.borderWidth t'
  , borderColor = T.borderColor t'

  , position = Top
  , height = 24

  , autoComplete = Just 10000
  , maxComplRows = Just 2
  , historySize = 100
  , historyFilter = id
  , searchPredicate = fuzzyMatch
  , alwaysHighlight = True
  }
  where t' = T.prompt . theme $ conf
