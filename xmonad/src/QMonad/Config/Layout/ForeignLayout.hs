{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module QMonad.Config.Layout.ForeignLayout (
  ForeignMessage(..),
  foreignLayout,
) where

import XMonad
import XMonad.Prelude
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

import QMonad.Config.Layout.LayoutDescriptionMeta

import Data.List (find)
import Data.Maybe (isJust)
import Control.Arrow (first, ArrowChoice (right))
import QMonad.Shared.XmobarColors (foreignWs)
import XMonad.Actions.CopyWindow (copyWindow, kill1)
import XMonad.Prompt (killBefore)

-- | Focus a foreign workspace
data ForeignLayout a = ForeignLayout (Maybe WorkspaceId) [Window] deriving (Show, Read)

data ForeignMessage = FocusForeign (Maybe WorkspaceId)
  | KillFocused
  deriving (Read, Show)

instance Message ForeignMessage

instance LayoutModifier ForeignLayout Window where
  modifyDescription (ForeignLayout (Just fws) _) = layoutMeta . Just $ "fws:" ++ fws
  modifyDescription _ = description

  handleMessOrMaybeModifyIt (ForeignLayout fws copied) m
    | Just (FocusForeign ws) <- fromMessage m = return . Just . Left $ ForeignLayout ws []
    | Just KillFocused <- fromMessage m = do
      focused <- W.peek <$> gets windowset
      case focused of
        Just w -> do
          when (w `elem` copied) (windows $ W.modify Nothing (W.filter (/= w)))
          kill1
          return . Just . Left $ ForeignLayout fws (filter (/= w) copied)
        _ -> return Nothing
    | otherwise = return Nothing

  modifyLayoutWithUpdate (ForeignLayout (Just fws) copied) ws r = do
      workspaces <- W.workspaces <$> gets windowset
      fl <- case find (\ws -> W.tag ws == fws) workspaces of
        Just fws -> do
          let wins = W.integrate' . W.stack $ ws
              fwins = W.integrate' . W.stack $ fws
              missing = filter (`notElem` wins) fwins
          unless (null missing) (windows $ W.modify (Just $ W.Stack (head missing) [] (tail missing))
                (\(W.Stack f l r) -> Just $ W.Stack f l (r ++ missing)))
          fst <$> runLayout fws fr
        _ -> return []
      let flw = map fst fl
      let stack' = W.filter (`notElem` flw) =<< W.stack ws
      (, Nothing) . first (++ fl) <$> runLayout ws { W.stack = stack' } mr
    where iw = rect_width r
          mw = round $ 0.6 * fromIntegral iw
          mr = r { rect_width = mw }
          fr = r { rect_x = rect_x r + fromIntegral mw, rect_width = iw - mw }
  modifyLayoutWithUpdate (ForeignLayout _ _) ws r = (, Nothing) <$> runLayout ws r

foreignLayout :: l a -> ModifiedLayout ForeignLayout l a
foreignLayout = ModifiedLayout $ ForeignLayout Nothing []

killFocused :: X()
killFocused = do
  ws <- W.workspace . W.current <$> gets windowset
  let (meta,_) = parseLayoutDescription [] (description . W.layout $ ws)
  if any ("fws" `isPrefixOf`) meta
    then sendMessage KillFocused
    else kill1
