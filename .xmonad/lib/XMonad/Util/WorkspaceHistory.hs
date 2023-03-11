
module XMonad.Util.WorkspaceHistory (
  workspaceHistoryLogHook,
  undoWorkspaceHistory,
  redoWorkspaceHistory
) where

import qualified Data.Map as M

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

------------------------------------------------------------------------------------------
-- Undo/Redo workspace for each Screen
------------------------------------------------------------------------------------------

data WorkspaceHistory = WorkspaceHistory {
      workspaceHistoryCurrent :: WorkspaceId,
      workspaceHistoryUndo :: [WorkspaceId],
      workspaceHistoryRedo :: [WorkspaceId]
} deriving ( Typeable, Read, Show )

data WorkspaceHistories = WorkspaceHistories (M.Map ScreenId WorkspaceHistory) deriving Typeable
instance ExtensionClass WorkspaceHistories where
  initialValue = WorkspaceHistories M.empty

workspaceHistoryLogHook sizeLimit = do
  WorkspaceHistories histories <- XS.get
  withWindowSet $ \ws -> do
      let screen = W.screen $ W.current ws
      let currentTag = W.tag $ W.workspace $ W.current ws
      if M.member screen histories then do
          let history = histories M.! screen
          let lastTag = workspaceHistoryCurrent history
          if currentTag == lastTag then
              return ()
          else do
              let undo = take sizeLimit $ lastTag : (workspaceHistoryUndo history)
              XS.put $ WorkspaceHistories $ M.insert screen (WorkspaceHistory currentTag undo []) histories
      else do
          XS.put $ WorkspaceHistories $ M.insert screen (WorkspaceHistory currentTag [] []) histories

undoWorkspaceHistory = do
  WorkspaceHistories histories <- XS.get
  withWindowSet $ \ws -> do
      let screen = W.screen $ W.current ws
      if M.member screen histories then do
          let history = histories M.! screen
          let undo = workspaceHistoryUndo history
          if undo == [] then return ()
          else do
              let x:xs = undo
              let redo = (workspaceHistoryCurrent history):(workspaceHistoryRedo history)
              XS.put $ WorkspaceHistories $ M.insert screen (WorkspaceHistory x xs redo) histories
              windows $ W.greedyView x
              -- Should we put it again? because log handler might be called while windows and it might put wrong history?
              -- XS.put $ WorkspaceHistories $ M.insert screen (WorkspaceHistory x xs redo) histories
      else return ()

redoWorkspaceHistory = do
  WorkspaceHistories histories <- XS.get
  withWindowSet $ \ws -> do
      let screen = W.screen $ W.current ws
      if M.member screen histories then do
          let history = histories M.! screen
          let redo = workspaceHistoryRedo history
          if redo == [] then return ()
          else do
              let x:xs = redo
              let undo = (workspaceHistoryCurrent history):(workspaceHistoryUndo history)
              XS.put $ WorkspaceHistories $ M.insert screen (WorkspaceHistory x undo xs) histories
              windows $ W.greedyView x
              -- Should we put it again? because log handler might be called while windows and it might put wrong history?
              -- XS.put $ WorkspaceHistories $ M.insert screen (WorkspaceHistory x undo xs) histories
      else return ()

------------------------------------------------------------------------------------------
-- Undo/Redo workspace for each Screen
------------------------------------------------------------------------------------------

