module XMonad.Util.WorkspaceFamily (
  WorkspaceInFamily(familyId, workspaceId),
  showWorkspaceInFamily, readWorkspaceInFamily,
  toFamilyIdMaybe, toWorkspaceIdMaybe,
  currentWorkspaceInFamily,
  expandWorkspacesToFamily,
  isSameWorkspaceFamily,
  doOnCurrentWorkspaceFamily,
  nextWS', prevWS', shiftToNextWS', shiftToPrevWS',
  compareWorkspaceAsWorkspaceFamily,
  greedyViewToWorkspace, greedyViewToFamily, greedyViewToFamilyWorkspace,
  shiftToWorkspace, shiftToFamily, shiftToFamilyWorkspace
) where

import qualified Data.Map.Strict as M
import Data.Ord
import Text.Read

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Actions.CycleWS
import qualified XMonad.Util.ExtensibleState as XS

import XMonad.Util.MyUtils

data WorkspaceInFamily = WorkspaceInFamily {
      familyId :: String,
      workspaceId :: String
}

showWorkspaceInFamily wif = show ("WIF", familyId wif, workspaceId wif)
readWorkspaceInFamily str =
  case readMaybe str :: Maybe (String, String, String) of
    Just ("WIF", family, id) -> Just WorkspaceInFamily {
                                  familyId = family,
                                  workspaceId = id
                                }
    _ -> Nothing
toFamilyIdMaybe str = readWorkspaceInFamily str >>= (Just . familyId)
toWorkspaceIdMaybe str = readWorkspaceInFamily str >>= (Just . workspaceId)

currentWorkspaceInFamily = readWorkspaceInFamily . W.tag . W.workspace . W.current

expandWorkspacesToFamily :: [String] -> [String] -> [String]
expandWorkspacesToFamily families ws = map showWorkspaceInFamily $ concat $ map toFamilies ws
  where toFamilies wsid = map (\fid -> WorkspaceInFamily {
                                         familyId = fid,
                                         workspaceId = wsid
                                       }) families

isSameWorkspaceFamily :: String -> X (WindowSpace -> Bool)
isSameWorkspaceFamily currentFamily =
    return (\ws ->
                case readWorkspaceInFamily $ W.tag ws of
                  Just wif -> familyId wif == currentFamily
                  Nothing -> False
           )
doOnCurrentWorkspaceFamily f = withWindowSet $ \s ->
  caseMaybeJust (currentWorkspaceInFamily s) $ f . WSIs . isSameWorkspaceFamily . familyId

nextWS' :: X ()
nextWS' = doOnCurrentWorkspaceFamily $ moveTo Next
prevWS' :: X ()
prevWS' = doOnCurrentWorkspaceFamily $ moveTo Prev

shiftToNextWS' :: X()
shiftToNextWS' = doOnCurrentWorkspaceFamily $ shiftTo Next
shiftToPrevWS' :: X()
shiftToPrevWS' = doOnCurrentWorkspaceFamily $ shiftTo Prev

compareWorkspaceAsWorkspaceFamily =
    compareByFamilyId `andThen` compareByWorkspaceId
                      where compareByFamilyId = comparing (toFamilyIdMaybe . W.tag)
                            compareByWorkspaceId = comparing (toWorkspaceIdMaybe . W.tag)

greedyViewToWorkspace wsid = withWindowSet $ \s ->
  caseMaybeJust (currentWorkspaceInFamily s) $ \wif ->
      windows $ W.greedyView $ showWorkspaceInFamily $ wif {workspaceId = wsid}

data FamilyWorkspaceMap = FamilyWorkspaceMap (M.Map (ScreenId, Maybe String) (Maybe String)) deriving Typeable
instance ExtensionClass FamilyWorkspaceMap where
  initialValue = FamilyWorkspaceMap M.empty
greedyViewToFamily fid =
    withWindowSet(\s -> do
      FamilyWorkspaceMap familyToWorkspace <- XS.get
      let wif = currentWorkspaceInFamily s
      XS.put $ FamilyWorkspaceMap $ M.insert (W.screen $ W.current s, wif >>= (Just . familyId)) (wif >>= (Just . workspaceId)) familyToWorkspace
      case M.lookup (W.screen $ W.current s, Just fid) familyToWorkspace of
        Just workspaceIdMaybe -> caseMaybeJust workspaceIdMaybe $ greedyViewToFamilyWorkspace fid
        Nothing -> caseMaybeJust (wif >>= (Just . workspaceId)) $ greedyViewToFamilyWorkspace fid
    )
greedyViewToFamilyWorkspace fid wsid = do
    windows $ W.greedyView $ showWorkspaceInFamily $ WorkspaceInFamily { familyId = fid, workspaceId = wsid }

shiftToWorkspace wsid = withWindowSet $ \s ->
  caseMaybeJust (currentWorkspaceInFamily s) $ \wif ->
    shiftToFamilyWorkspace (familyId wif) $ wsid
shiftToFamily fid = withWindowSet $ \s ->
  caseMaybeJust (currentWorkspaceInFamily s) $ \wif ->
    shiftToFamilyWorkspace fid $ workspaceId wif
shiftToFamilyWorkspace fid wsid =
  windows $ W.shift $ showWorkspaceInFamily $ WorkspaceInFamily { familyId = fid, workspaceId = wsid }
