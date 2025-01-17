{-# LANGUAGE DeriveDataTypeable #-}

module QMonad.Lib.WorkspaceMasks (
    WorkspaceMasks(..),
    WorkspaceMask(..),
    WSPP(..),
    WSMaskFormat,

    workspaceMasksPP,

    getWorkspaceMasks',
    getWorkspaceMasks,
    setWorkspaceMask,
    setCurrentWorkspaceMask,

    getWorkspaceName,
    getWorkspaceVisibility,
    getCurrentWorkspaceName,
    getCurrentWorkspaceVisibility,

    toggleCurrentWorkspaceVisibility,
    renameCurrentWorkspace,
) where

-- Imports/ Base
import XMonad
import Data.Maybe (fromMaybe)
import qualified Data.Map        as M
import qualified XMonad.StackSet as W

-- Imports/ Prompt
import XMonad.Prompt (mkXPrompt, XPConfig)
import XMonad.Prompt.Workspace (Wor(Wor))

-- Imports/ Uncategorized
import XMonad.Hooks.DynamicLog (PP(..))
import qualified XMonad.Util.ExtensibleState as XS


type MaskState = (String, Bool)
newtype WorkspaceMasks = WorkspaceMasks (M.Map WorkspaceId MaskState)
    deriving (Read, Show, Typeable)

instance ExtensionClass WorkspaceMasks where
    initialValue = WorkspaceMasks M.empty
    extensionType = PersistentExtension

data WorkspaceMask = WorkspaceMask WorkspaceId Bool String

type WSMaskFormat = WorkspaceMask -> (WorkspaceId -> String) -> String
data WSPP = WSPP { wsppCurrent :: WSMaskFormat
                 , wsppVisible :: WSMaskFormat
                 , wsppHidden :: WSMaskFormat
                 , wsppHiddenNoWindows :: WSMaskFormat
                 , wsppUrgent :: WSMaskFormat
                 }

instance Default WSPP where
    def = WSPP { wsppCurrent = fallbackWSPP
               , wsppVisible = fallbackWSPP
               , wsppHidden = fallbackWSPP
               , wsppHiddenNoWindows = fallbackWSPP
               , wsppUrgent = fallbackWSPP
               }

fallbackWSPP :: WSMaskFormat
fallbackWSPP (WorkspaceMask wks _ _) s = s wks

workspaceMasksPP :: PP -> WSPP -> X PP
workspaceMasksPP pp wspp = do
    masks <- getWorkspaceMasks
    return $ pp {
            ppCurrent = \wks -> wsppCurrent wspp (masks wks) (ppCurrent pp)
        ,   ppVisible  = \wks -> wsppVisible wspp (masks wks) (ppVisible pp)
        ,   ppHidden  = \wks -> wsppHidden wspp (masks wks) (ppHidden pp)
        ,   ppHiddenNoWindows = \wks -> wsppHiddenNoWindows wspp (masks wks) (ppHiddenNoWindows pp)
        ,   ppUrgent = \wks -> wsppUrgent wspp (masks wks) (ppUrgent pp)
        }

getWorkspaceMasks' :: X (WorkspaceId -> Maybe MaskState)
getWorkspaceMasks' = do
    WorkspaceMasks m <- XS.get
    return (`M.lookup` m)

getWorkspaceMasks :: X (WorkspaceId -> WorkspaceMask)
getWorkspaceMasks = do
    lookup <- getWorkspaceMasks'
    return $ \wks -> case lookup wks of
        Just (n,v) -> WorkspaceMask wks v n
        _ -> WorkspaceMask wks True wks

setWorkspaceMask :: WorkspaceId -> Maybe String -> Maybe Bool -> X ()
setWorkspaceMask w name' visible' = do
    WorkspaceMasks m <- XS.get
    let current = M.findWithDefault ([],True) w m
    let name    = fromMaybe (fst current) name'
        visible = fromMaybe (snd current) visible'
    XS.put $ WorkspaceMasks $ if null name && visible
        then M.delete w m
        else M.insert w (if null name then w else name, visible) m
    refresh

setCurrentWorkspaceMask :: Maybe String -> Maybe Bool -> X ()
setCurrentWorkspaceMask name visible = do
    current <- gets (W.currentTag . windowset)
    setWorkspaceMask current name visible

getWorkspaceName :: WorkspaceId -> X String
getWorkspaceName w = do
    WorkspaceMasks m <- XS.get
    return $ fst $ M.findWithDefault ([],True) w m

getWorkspaceVisibility :: WorkspaceId -> X Bool
getWorkspaceVisibility w = do
    WorkspaceMasks m <- XS.get
    return $ snd $ M.findWithDefault ([],True) w m

getCurrentWorkspaceName :: X String
getCurrentWorkspaceName = getWorkspaceName =<< gets (W.currentTag . windowset)

getCurrentWorkspaceVisibility :: X Bool
getCurrentWorkspaceVisibility = getWorkspaceVisibility =<< gets (W.currentTag . windowset)

toggleCurrentWorkspaceVisibility :: X ()
toggleCurrentWorkspaceVisibility = do
    current <- gets (W.currentTag . windowset)
    WorkspaceMasks m <- XS.get
    let visible = snd $ M.findWithDefault ([],True) current m
    setWorkspaceMask current Nothing (Just $ not visible)

renameCurrentWorkspace :: XPConfig -> X ()
renameCurrentWorkspace conf = do
    mkXPrompt pr conf (const (return [])) (\s -> setCurrentWorkspaceMask (Just s) Nothing)
    where pr = Wor "Workspace name: "

