{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module QMonad.Config.Hooks.Layouts (
  layoutHook,
  FocalChange(..),
  FocalToggle(..),
) where

import XMonad hiding (layoutHook)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Layout.BoringWindows (boringWindows)
import XMonad.Layout.Minimize (minimize)
import XMonad.Layout.Named (named)
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Spacing (spacingRaw, Border(..))
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

import Data.List (find)
import Data.Maybe (isJust)

-- Custom layouts

-- | Temporarily make a window mimic Full layout
newtype FocalWindow a = FocalWindow (Maybe Window) deriving (Show, Read)

newtype FocalChange = FocalChange (Maybe Window) deriving (Typeable)
newtype FocalToggle = FocalToggle Window deriving (Typeable)

instance Message FocalChange
instance Message FocalToggle

instance LayoutModifier FocalWindow Window where
  pureMess (FocalWindow cw) m
      | Just (FocalChange w) <- fromMessage m = Just $ FocalWindow w
      | Just (FocalToggle w) <- fromMessage m = Just . FocalWindow $ case cw of
            Just cw' -> if w /= cw'
                then Just w
                else Nothing
            _ -> Just w
      | otherwise = Nothing

  modifyLayout (FocalWindow (Just w)) ws r = if isJust stack
      then runLayout ws { W.stack = stack } r
      else runLayout ws r
    where wins = W.integrate' . W.stack $ ws
          stack | isJust $ find (== w) wins = Just W.Stack { W.focus = w, W.up = [], W.down = [] }
                | otherwise = Nothing
  modifyLayout (FocalWindow _) ws r = runLayout ws r

focalWindow :: l a -> ModifiedLayout FocalWindow l a
focalWindow = ModifiedLayout $ FocalWindow Nothing

-- Layout hook
windowSpacing = 4 `div` 2
screenSpacing = [ 6 - windowSpacing -- Top
                , 6 - windowSpacing -- Bottom
                , 6 - windowSpacing -- Right
                , 6 - windowSpacing -- Left
                ]

mkGaps = spacingRaw False
  (Border (head screenSpacing) (screenSpacing!!1) (screenSpacing!!2) (screenSpacing!!3)) True
  (Border windowSpacing windowSpacing windowSpacing windowSpacing) True

layouts = named "V 2*|1" vtall
      ||| named "H 1|1" htall
      ||| named "1" full
      ||| named "FS" fullScreen
  where
    defaultIncr = 5/100
    vtall = focalWindow . avoidStruts . mkGaps $ Mirror $ Tall 2 defaultIncr (2/3)
    htall = focalWindow . avoidStruts . mkGaps $ Tall 1 defaultIncr (1/2)
    full  = avoidStruts . mkGaps . noBorders $ Full
    fullScreen = noBorders Full

layoutHook = minimize . boringWindows . smartBorders $ layouts
