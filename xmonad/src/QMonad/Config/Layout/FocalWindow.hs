{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module QMonad.Config.Layout.FocalWindow (
  FocalChange(..),
  FocalToggle(..),
  focalWindow,
) where

import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

import QMonad.Config.Layout.LayoutDescriptionMeta

import Data.List (find)
import Data.Maybe (isJust)

-- | Temporarily make a window mimic Full layout
newtype FocalWindow a = FocalWindow (Maybe Window) deriving (Show, Read)

newtype FocalChange = FocalChange (Maybe Window) deriving (Typeable)
newtype FocalToggle = FocalToggle Window deriving (Typeable)

instance Message FocalChange
instance Message FocalToggle

instance LayoutModifier FocalWindow Window where
  modifyDescription (FocalWindow (Just _)) = layoutMeta (Just "focal")
  modifyDescription _ = description

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
