{-# LANGUAGE DeriveGeneric #-}

module QMonad.Config.Theme (
    Theme(..),
    ThemeBorder(..),
    ThemePrompt(..),

    defaultTheme',
) where

import GHC.Generics
import Data.Yaml (FromJSON)

data Theme = Theme {
    border :: ThemeBorder,
    prompt :: ThemePrompt
  } deriving (Read, Show, Generic)

data ThemeBorder = ThemeBorder {
    width :: Int,
    normal :: String,
    focused :: String
  } deriving (Read, Show, Generic)

data ThemePrompt = ThemePrompt {
    font :: String,
    fontSize :: Int,

    foreground :: String,
    background :: String,
    selectedForeground :: String,
    selectedBackground :: String,

    borderWidth :: Int,
    borderColor :: String
  } deriving (Read, Show, Generic)

instance FromJSON Theme
instance FromJSON ThemeBorder
instance FromJSON ThemePrompt

defaultTheme' = Theme {
    border = ThemeBorder {
        width = 1,
        normal = "black",
        focused = "white"
      },
    prompt = ThemePrompt {
        font = "Ubuntu",
        fontSize = 10,

        foreground = "white",
        background = "black",
        selectedForeground = "white",
        selectedBackground = "blue",

        borderWidth = 0,
        borderColor = "black"
      }
  }
