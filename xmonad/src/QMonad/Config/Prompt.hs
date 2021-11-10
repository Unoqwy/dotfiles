module QMonad.Config.Prompt (
    defaultConfig
) where

import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch (fuzzyMatch)

import qualified QMonad.Shared.Theme as T

defaultConfig :: XPConfig
defaultConfig = def {
    font     = T.secondFont 11
  , bgColor  = T.bgColor
  , fgColor  = T.fgColor
  , bgHLight = T.primaryColor
  , fgHLight = T.fgOnPrimColor

  , borderColor       = T.primaryColor
  , promptBorderWidth = 1

  , position = Top
  , height   = 24

  , autoComplete = Just 10000
  , maxComplRows = Just 2
  , historySize   = 100
  , historyFilter = id
  , searchPredicate = fuzzyMatch
  , alwaysHighlight = True
  }

