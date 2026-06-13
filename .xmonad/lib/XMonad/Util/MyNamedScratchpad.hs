
module XMonad.Util.MyNamedScratchpad (
  terminalScratchpad,
  namedScratchpadHandleEventHook,
  keepWindowSizeHandleEventHook,
  myNamedScratchpadActionInternal,
  toggleScrachpadAction,
  runScratchpadAction
) where

import qualified Data.List as L
import Data.Monoid

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad2
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.HandleEventHooks

import XMonad.Actions.TerminalAction

------------------------------------------------------------------------------------------
-- Scratchpad
------------------------------------------------------------------------------------------

terminalScratchpad :: Terminal t => t -> String -> Maybe String -> ManageHook -> NamedScratchpad
terminalScratchpad term name execMaybe manageHook =
    NS name
       (startTerminalCommand term id execMaybe)
       (className =? id)
       manageHook
    where id = "xmonad.namedscratchpad." ++ name

data NamedScratchpadSendEventWindows = NamedScratchpadSendEventWindows [Window] deriving Typeable
instance ExtensionClass NamedScratchpadSendEventWindows where
  initialValue = NamedScratchpadSendEventWindows []

namedScratchpadHandleEventHook scratchpads = keepWindowSizeHandleEventHook $ L.foldr (<||>) (return False) $ L.map query scratchpads

myNamedScratchpadActionInternal scratchpads n = do
    namedScratchpadAction scratchpads n
    myNamedScratchpadRelocationAction scratchpads n

myNamedScratchpadRelocationAction scratchpads n = do
      withWindowSet (\s ->
         case (W.peek s, L.find isSameName scratchpads) of
           (Just w, Just ns) -> do
             hook <- runQuery (hook ns) w
             isTarget <- runQuery (query ns) w
             case isTarget of
               True -> windows(\ws -> appEndo hook ws)
               _ -> return ()
           _ -> return ()
       )
  where
    isSameName = \ns -> n == (name ns)

runScratchpadAction scratchpad = myNamedScratchpadActionInternal [scratchpad] $ name scratchpad

toggleScrachpadAction scratchpads =
    withWindowSet (\s ->
      case W.peek s of
        Just w -> do
          (cur, next) <- findCurrentAndNextScrachpadOf scratchpads w
          myNamedScratchpadActionMaybe cur >> myNamedScratchpadActionMaybe next
        _ -> doAction $ name (head scratchpads)
    )
  where doAction = myNamedScratchpadActionInternal scratchpads
        myNamedScratchpadActionMaybe mns =
            whenJust mns $ \ns -> doAction $ name ns

findCurrentAndNextScrachpadOf nss w =
    findIt (cycle ((asMaybe nss) ++ [Nothing]))
  where
    asMaybe = \l -> map Just l
    findIt = \l ->
               let
                   h = head l
                   t = tail l
               in
                 case h of
                   Just ns -> do
                          matched <- runQuery (query ns) w
                          if matched then
                              return (h, head t)
                          else
                              findIt t
                   _ -> return (h, head t)

isScratchpadWindow (s:ss) w = do
  isTarget <- runQuery (query s) w
  case isTarget of
    True -> return True
    False -> isScratchpadWindow ss w

------------------------------------------------------------------------------------------
-- Scratchpad
------------------------------------------------------------------------------------------
