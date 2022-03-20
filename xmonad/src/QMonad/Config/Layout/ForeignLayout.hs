{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

module QMonad.Config.Layout.ForeignLayout (
  ForeignMessage(..),
  foreignLayout,
  killFocused,
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

-- | Bring a foreign workspace as a subset of the current workspace
data ForeignLayout a = ForeignLayout {
    wid :: Maybe WorkspaceId
  , fws :: Maybe WorkspaceId
  , copied :: [Window]
  , split :: Float
  } deriving (Show, Read)

data ForeignMessage = PopulateBoundWs WorkspaceId
  | ResizeForeignSplit Float
  | FocusForeign (Maybe WorkspaceId)
  | PrepareForRemoval WorkspaceId Window
  deriving (Read, Show)

instance Message ForeignMessage

instance LayoutModifier ForeignLayout Window where
  modifyDescription (ForeignLayout _ (Just fws) _ _) = layoutMeta . Just $ "fws:" ++ fws
  modifyDescription _ = description

  handleMessOrMaybeModifyIt fl@(ForeignLayout wid fws copied _) m
    | Just (PopulateBoundWs ws) <- fromMessage m = do
      update $ fl { wid = Just ws }
    | Just (ResizeForeignSplit by) <- fromMessage m = do
      update $ fl { split = max 0.1 $ min (split fl + by) 0.9 }
    | Just (FocusForeign ws) <- fromMessage m = do
      filterWindows (`notElem` copied)
      let ws' = (\ws -> if ws == fromJust wid then Nothing else Just ws) =<< ws in
        update $ fl { fws = ws', copied = [] }
    | Just (PrepareForRemoval fws' w) <- fromMessage m = if Just fws' == fws && w `elem` copied
      then do
        filterWindows (/= w)
        update $ fl { copied = filter (/= w) copied }
      else return Nothing
    | otherwise = return Nothing
    where filterWindows f = modifyWindowSet $ \wset -> W.view (W.currentTag wset) $
           W.modify Nothing (W.filter f) $ W.view (fromJust wid) wset
          update = return . Just . Left

  modifyLayoutWithUpdate l@(ForeignLayout (Just _) (Just fws) copied split) ws r = do
      workspaces <- W.workspaces <$> gets windowset
      (fl, c) <- case find (\ws -> W.tag ws == fws) workspaces of
        Just fws -> do
          let wins = W.integrate' . W.stack $ ws
              fwins = W.integrate' . W.stack $ fws
              missing = filter (`notElem` wins) fwins
          unless (null missing) (windows $ W.modify (Just $ W.Stack (head missing) [] (tail missing))
                (\(W.Stack f l r) -> Just $ W.Stack f l (r ++ missing)))
          fl <- fst <$> runLayout fws fr
          return (fl, Just $ copied ++ filter (`notElem` copied) missing)
        _ -> return ([], Nothing)
      let flw = map fst fl
      let stack' = W.filter (`notElem` flw) =<< W.stack ws
      (, fmap (\c -> l { copied = c }) c) . first (++ fl) <$> runLayout ws { W.stack = stack' } mr
    where iw = rect_width r
          mw = round $ (1.0 - split) * fromIntegral iw
          mr = r { rect_width = mw }
          fr = r { rect_x = rect_x r + fromIntegral mw, rect_width = iw - mw }
  modifyLayoutWithUpdate ForeignLayout {} ws r = (, Nothing) <$> runLayout ws r

foreignLayout :: l a -> ModifiedLayout ForeignLayout l a
foreignLayout = ModifiedLayout $ ForeignLayout Nothing Nothing [] 0.4

killFocused :: X()
killFocused = do
  withFocused $ \w -> do
    ws <- W.tag . W.workspace . W.current <$> gets windowset
    broadcastMessage $ PrepareForRemoval ws w
  kill1
