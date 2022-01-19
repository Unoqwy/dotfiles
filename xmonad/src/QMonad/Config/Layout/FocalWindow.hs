{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

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

  modifyLayoutWithUpdate (FocalWindow (Just w)) ws r = do
      let wins = W.integrate' . W.stack $ ws
      let (run, state) = case find (== w) wins of
            Just w -> do
               let stack = Just $ W.Stack w [] []
               (runLayout ws { W.stack = stack } r, Nothing)
            _ -> (runLayout ws r, Just $ FocalWindow Nothing)
      (, state) <$> run
  modifyLayoutWithUpdate (FocalWindow _) ws r = (, Nothing) <$> runLayout ws r

focalWindow :: l a -> ModifiedLayout FocalWindow l a
focalWindow = ModifiedLayout $ FocalWindow Nothing
