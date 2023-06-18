module QMonad.Config.Aggregate (
  WorkspaceAggregate(..),
  QMonad.Config.Aggregate.workspaces,
) where

import XMonad
import XMonad.Prelude hiding (empty)
import XMonad.Util.Minimize (Minimized(minimizedStack))
import XMonad.Util.WorkspaceCompare (getSortByIndex)
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

import qualified Data.Map as M

import QMonad.Config.Layout.LayoutDescriptionMeta (parseLayoutDescription)
import QMonad.Lib.WorkspaceMasks

data WorkspaceAggregate = WorkspaceAggregate {
    idx :: Int,
    tag :: WorkspaceId,
    name :: String,
    visible :: Bool,
    empty :: Bool,
    hasMinimized :: Bool,
    focal :: Bool
  }

workspaces :: X [WorkspaceAggregate]
workspaces = do
  sort' <- getSortByIndex
  minimized <- XS.gets minimizedStack
  stackWs <- sort' . W.workspaces <$> gets windowset
  WorkspaceMasks masks <- XS.get
  return $ zipWith
    (\idx ws  -> do
      let stack = W.integrate' . W.stack $ ws
      let (meta,_) = parseLayoutDescription [] (description . W.layout $ ws)
      let wsTag = W.tag ws
      let mask = M.findWithDefault ([], True) wsTag masks
      WorkspaceAggregate {
        idx = idx,
        tag = wsTag,
        name = fst mask,
        visible = snd mask,
        empty = not $ any (`notElem` minimized) stack,
        hasMinimized = any (`elem` minimized) stack,
        focal = "focal" `elem` meta
      })
    [0..] stackWs

