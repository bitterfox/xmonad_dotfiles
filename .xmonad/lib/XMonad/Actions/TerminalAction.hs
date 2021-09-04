
module XMonad.Actions.TerminalAction (
  Terminal(..),
  TerminalAction(..),
  (.<.),
  (.>.),
  (.>|),
  (.>>),
  (.>?),
  (.|),
  (.||),
  (.||=),
  withFirstLine,
  withoutEmpty,
  terminalActionTemplate,
  selectedXTerminalAction,
  spawnSelectedAppTerminalAction,
  runSelectedXTerminalAction
) where

import System.Directory
import System.IO
import System.IO.Error hiding (catch)
import qualified Data.List as L
import Control.Exception.Extensible as E

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

------------------------------------------------------------------------------------------
-- Terminal actions
------------------------------------------------------------------------------------------

data CurrentTerminalAction = CurrentTerminalAction (Maybe (TerminalAction (), String, String, CurrentTerminalActionState)) deriving Typeable
instance ExtensionClass CurrentTerminalAction where
  initialValue = CurrentTerminalAction Nothing

data CurrentTerminalActionState = Initialize | Started deriving Typeable

data TerminalAction o = TerminalAction {
      actionName :: String,
      actionInputs :: X [String],
      actionScript :: String,
      actionOutputsHandler :: [String] -> X o,
      actionManageHook :: ManageHook
} deriving Typeable

(.<.) :: TerminalAction a -> X [String] -> TerminalAction a
ta .<. inputs = ta { actionInputs = inputs }

(.>.) :: TerminalAction a -> (a -> X b) -> TerminalAction b
ta .>. handler = ta {
  actionOutputsHandler = \outputs -> do
    o <- actionOutputsHandler ta outputs
    handler o
}

(.>|) :: TerminalAction a -> b -> TerminalAction b
ta .>| b = ta {
  actionOutputsHandler = \outputs -> do
    o <- actionOutputsHandler ta outputs
    return b
}

(.|) :: TerminalAction a -> (a -> b) -> TerminalAction b
ta .| handler = ta {
  actionOutputsHandler = \outputs -> do
    o <- actionOutputsHandler ta outputs
    return $ handler o
}

(.||) :: TerminalAction (Maybe a) -> (a -> b) -> TerminalAction (Maybe b)
ta .|| handler = ta {
  actionOutputsHandler = \outputs -> do
    mo <- actionOutputsHandler ta outputs
    return $ mo >>= (Just . handler)
}

(.||=) :: TerminalAction (Maybe a) -> (a -> Maybe b) -> TerminalAction (Maybe b)
ta .||= handler = ta {
  actionOutputsHandler = \outputs -> do
    mo <- actionOutputsHandler ta outputs
    return $ mo >>= handler
}

(.>>) :: TerminalAction (Maybe a) -> (a -> X ()) -> TerminalAction ()
ta .>> handler = ta {
  actionOutputsHandler = \outputs -> actionOutputsHandler ta outputs >>= doForJust handler
}
  where doForJust f m = whenJust m f

(.>?) :: TerminalAction (Maybe a) -> X () -> TerminalAction (Maybe a)
ta .>? handler = ta {
  actionOutputsHandler = \outputs -> do
    mo <- actionOutputsHandler ta outputs
    case mo of
      Nothing -> handler
      _ -> return ()
    return mo
}

withoutEmpty outputs
    | outputs == [] = Nothing
    | otherwise = Just outputs

withFirstLine outputs
    | outputs == [] = Nothing
    | otherwise = Just $ head outputs

terminalActionTemplate :: String -> String -> ManageHook -> TerminalAction [String]
terminalActionTemplate name script manageHook =
  TerminalAction {
    actionName = name,
    actionInputs = return [],
    actionScript = script,
    actionOutputsHandler = return,
    actionManageHook = manageHook
  }

class Terminal t where
    terminalQuery :: t -> TerminalAction () -> Query Bool
    runNamedTerminalAction :: t -> [TerminalAction ()] -> String -> X ()
    runNamedTerminalAction t as name =
      whenJust (L.find ((name ==) . actionName) as) $ runTerminalAction t

    runTerminalAction :: t -> TerminalAction () -> X ()
    runTerminalAction t a@TerminalAction{actionName = name, actionInputs = inputs} = do
      CurrentTerminalAction ma <- XS.get
      case ma of
        Just (a', i', o', Initialize) -> XS.put $ CurrentTerminalAction Nothing
        Just (a', i', o', Started) -> closeTerminalAction t a' i' o' True
        _ -> return ()
      let inFile = "/tmp/xmonad.terminal.action." ++ name ++ ".in"
      let outFile = "/tmp/xmonad.terminal.action." ++ name ++ ".out"
--      XS.put $ CurrentTerminalAction $ Just (actionName a, inFile, outFile, Initialize)
      XS.put $ CurrentTerminalAction $ Just (a, inFile, outFile, Initialize)
      lines <- inputs
      io $ writeFile inFile $ unlines lines
      startTerminal t a inFile outFile

    startTerminal :: t -> TerminalAction () -> FilePath -> FilePath -> X ()

    terminalManageHook :: t -> [TerminalAction ()] -> ManageHook
    terminalManageHook t as = composeAll $ L.map (\a -> (terminalQuery t a) --> (actionManageHook a)) as

    terminalLogHook :: t -> [TerminalAction ()] -> X ()
    terminalLogHook t as = do
      CurrentTerminalAction ma <- XS.get
      case ma of
        Just (action, inFile, outFile, Initialize) ->
          withFocused $ \fw -> do
            whenX (runQuery (terminalQuery t action) fw) $ do
              XS.put $ CurrentTerminalAction $ Just (action, inFile, outFile, Started)
              dontBorder fw
        Just (action, inFile, outFile, Started) ->
          withWindowSet $ \ws -> do
            let stack = W.stack $ W.workspace $ W.current ws
            case stack of
              Just (W.Stack {W.focus = fw}) -> do
                isTarget <- runQuery (terminalQuery t action) fw
                whenX (return $ not isTarget) $ do
                  closeTerminalAction t action inFile outFile True
              Nothing -> closeTerminalAction t action inFile outFile True
        _ -> return ()
       where findTerminalAction as name = L.find ((name ==) . actionName) as
             dontBorder w = withDisplay $ \dpy -> io $ setWindowBorderWidth dpy w 0

    closeTerminalAction :: t -> TerminalAction () -> FilePath -> FilePath -> Bool -> X ()
    closeTerminalAction t a inFile outFile runOutputHandler = do
      XS.put $ CurrentTerminalAction Nothing
      let q = terminalQuery t a
      killMatchedWindow q
      whenX (return runOutputHandler) $ do
        outLines <- io $ safeReadFile outFile
        actionOutputsHandler a $ lines outLines
      io $ safeRemoveFile inFile
      io $ safeRemoveFile outFile
      where handleExists d e
                         | isDoesNotExistError e = return d
                         | otherwise = throwIO e
            safeRemoveFile fileName = removeFile fileName `catch` handleExists ()
            safeReadFile fileName = readFile fileName `catch` handleExists []
            killMatchedWindow query =
              withWindowSet $ \ws ->
                L.foldr (>>) (return ()) $ L.map (\w -> whenX (runQuery query w) $ killWindow w) $ W.allWindows ws

selectedXTerminalAction template xs =
    template .<. (return $ L.map fst xs)
             .||= (\a -> L.find ((a ==) . fst) xs)
             .>> snd

spawnSelectedAppTerminalAction template apps =
    selectedXTerminalAction template $ L.map (\(a, b) -> (a, spawn b)) apps

runSelectedXTerminalAction terminal template xs =
    runTerminalAction terminal $ selectedXTerminalAction template xs

------------------------------------------------------------------------------------------
-- Terminal actions
------------------------------------------------------------------------------------------
