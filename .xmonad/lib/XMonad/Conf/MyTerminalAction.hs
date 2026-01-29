module XMonad.Conf.MyTerminalAction where

import qualified Data.Text as T
import qualified Data.List as L
import Data.Maybe

import Control.Exception.Extensible as E

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Actions.TerminalAction
import XMonad.Actions.TerminalAction.GnomeBackend

import XMonad.Util.ManageHookUtils
import XMonad.Util.HandleEventHooks
import XMonad.Util.WorkspaceFamily
import XMonad.Util.MyUtils
import XMonad.Conf.MyConf

terminalActionManageHook = onCenter'' 0.3 0.2

myTerminal = GnomeTerminal "xmonad.terminal.action"
selectWindowTerminalActionTemplate =
  (terminalActionTemplate "select.window" "~/.xmonad/terminal_actions/select_window.sh" $ onCenter'' 0.1 0.2)
  .| withFirstLine .|| words .|| head .|| read
selectActionTerminalActionTemplate =
  (terminalActionTemplate "select.action" "~/.xmonad/terminal_actions/select_action.sh" terminalActionManageHook)
  .| withFirstLine
dmenuRunTerminalAction =
  (terminalActionTemplate "dmenu.run" "~/.xmonad/terminal_actions/dmenu_run.sh" terminalActionManageHook)
  .| withFirstLine .>> spawn
openBrowserHistoryTerminalAction =
  (terminalActionTemplate "open.browser.history" "~/.xmonad/terminal_actions/select_browser_history.sh" $ onCenter'' 0.1 0.2)
  .| withFirstLine .|| (\s -> "xdg-open '" ++ s ++ "'") .>> spawn
openBrowserTabTerminalAction =
  (terminalActionTemplate "open.browser.tab" "~/.xmonad/terminal_actions/select_browser_tab.sh" $ onCenter'' 0.1 0.2) .>| ()
copyFromClipboardHistoryListTerminalAction =
  (terminalActionTemplate "copy.from.clipboard.history.list" "~/.xmonad/terminal_actions/select_clipboard.sh" $ terminalActionManageHook) .>| ()
copyFromClipboardHistoryAgTerminalAction =
  (terminalActionTemplate "copy.from.clipboard.history.ag" "~/.xmonad/terminal_actions/select_clipboard.sh ag" $ terminalActionManageHook) .>| ()
copyFromClipboardHistoryListMultiTerminalAction =
  (terminalActionTemplate "copy.from.clipboard.history.list.multi" "~/.xmonad/terminal_actions/select_clipboard.sh multi" $ terminalActionManageHook) .>| ()
onePasswordTerminalAction =
  (terminalActionTemplate "one.password" "~/.xmonad/terminal_actions/one_password.sh" $ onCenter'' 0.1 0.2)
  .| withFirstLine .|| (\s -> "xdotool type '" ++ (T.unpack $ T.replace (T.pack "'") (T.pack "'\"'\"'") (T.pack s)) ++ "'") .>> spawn
openDashboardTerminalAction =
  (terminalActionTemplate "open.dashboard" "" terminalActionManageHook)
  .| withoutEmpty .|| (\outputs ->
                           if head outputs == "alt-enter" then
                               (False, tail outputs)
                           else
                               (True, outputs))
  .|| (\(appMode, urls) -> L.map (\url -> (if appMode then "vivaldi --app='" else "xdg-open '") ++ url ++ "'") urls)
  .|| L.map spawn
  .>> L.foldr (>>) (return ())

myTerminalActions = [
   (terminalActionTemplate "open.intellij" "~/.xmonad/terminal_actions/open_intellij.sh" terminalActionManageHook)
   .| withFirstLine .|| ((intellijCommand ++ " ") ++) .>> spawn
  , dmenuRunTerminalAction
  , openBrowserHistoryTerminalAction
  , openBrowserTabTerminalAction
  , copyFromClipboardHistoryListTerminalAction
  , copyFromClipboardHistoryAgTerminalAction
  , copyFromClipboardHistoryListMultiTerminalAction
  , onePasswordTerminalAction
  , selectWindowTerminalActionTemplate .>| ()
  , selectActionTerminalActionTemplate .>| ()
  , openDashboardTerminalAction]

myTerminalActionHandleEventHook = keepWindowSizeHandleEventHook $ L.foldr (<||>) (return False) $ L.map (terminalQuery myTerminal) myTerminalActions

openIntelliJTerminalAction = do
  runNamedTerminalAction myTerminal myTerminalActions "open.intellij"

data BrowserHistoryTerminalActionState = BrowserHistoryTerminalActionState Int deriving (Typeable)
instance ExtensionClass BrowserHistoryTerminalActionState where
  initialValue = BrowserHistoryTerminalActionState 0
runOpenBrowserHistoryTerminalAction = do
  let sort = ["often", "recent"]
  runCyclicTerminalAction myTerminal "open.browser.history" $
                          L.map (openBrowserHistoryTerminalAction .<) sort

runOpenBrowserTabTerminalAction = do
  runNamedTerminalAction myTerminal myTerminalActions "open.browser.tab"

runCopyFromClipboardHistoryTerminalAction = do
  runCyclicTerminalAction myTerminal "copy.from.clipboard.history" $
                          [copyFromClipboardHistoryListTerminalAction, copyFromClipboardHistoryAgTerminalAction, copyFromClipboardHistoryListMultiTerminalAction]

runOnePasswordTerminalAction = do
  runNamedTerminalAction myTerminal myTerminalActions "one.password"

data SelectedWindowTerminalActionState = SelectedWindowTerminalActionState String Int deriving (Typeable)
instance ExtensionClass SelectedWindowTerminalActionState where
  initialValue = SelectedWindowTerminalActionState "" 0

smartGreedyViewSelectedWindowTerminalAction =
  runSelectedWindowTerminalAction "smart.greedy.view" smartGreedyViewWindow

greedyViewSelectedWindowTerminalAction =
  runSelectedWindowTerminalAction "greedy.view" greedyViewWindow

shiftSelectedWindowTerminalAction =
  runSelectedWindowTerminalAction "shift" $ \w -> windows $ \s -> W.shiftMaster $ W.focusWindow w $ W.shiftWin (W.currentTag s) w s

runSelectedWindowTerminalAction myname handler predicates = do
    s <- gets windowset
    let w = W.peek s
    runCyclicTerminalAction myTerminal myname $
                            L.map (\(header, predicate) -> selectWindowTerminalAction w header predicate .>> handler) predicates

selectWindowTerminalAction w header predicate  = do
  selectWindowTerminalActionTemplate .<. ((windowMap' predicate) >>= (\l -> return $ ((maybe "0" show w) ++ " " ++ header):(L.map (\(s, w) -> (show w) ++ " " ++ s) l)))
windowMap' :: (WindowSet -> WindowSpace -> Bool) -> X [(String,Window)]
windowMap' predicate = do
    ws <- gets windowset
    let workspaces = L.sortBy compareWorkspaceAsWorkspaceFamily $ W.workspaces ws
    let windows = foldr (++) [] $ map (W.integrate' . W.stack) $ filter (predicate ws) workspaces
    maxClassLength <- fmap (foldr max 0) $ mapM (fmap length . getClass') windows
    wins <- mapM (keyValuePair maxClassLength ws) windows
    return wins
 where keyValuePair maxClassLength ws w = flip (,) w `fmap` (decorateName' maxClassLength ws w)
decorateName' :: Int -> WindowSet -> Window -> X String
decorateName' maxClassLength ws w = do
  name <- getName' w
  clazz <- getClass' w
  workspace <- getWorkspace' w
  let workspaces = W.workspaces ws
  let focuses = L.map W.focus $ catMaybes $ L.map (W.stack) workspaces
  let classifier = if L.elem w focuses then "* " else "  "
  return ("[" ++ workspace ++ "] " ++ classifier ++ clazz ++ (replicate (maxClassLength - (length clazz)) ' ') ++ " : " ++ name)

getName' :: Window -> X String
getName' w = withDisplay $ \d -> do
    -- TODO, this code is ugly and convoluted -- clean it up
    let getIt = bracket getProp (xFree . tp_value) (copy)
        getProp = (internAtom d "_NET_WM_NAME" False >>= getTextProperty d w)
                      `E.catch` \(SomeException _) -> getTextProperty d w wM_NAME
        copy prop = fromMaybe "" . listToMaybe <$> wcTextPropertyToTextList d prop
    io $ getIt `E.catch` \(SomeException _) ->  (resName) `fmap` getClassHint d w
getClass' :: Window -> X String
getClass' w = withDisplay $ \d -> do
    -- TODO, this code is ugly and convoluted -- clean it up
    let getIt = bracket getProp (xFree . tp_value) (copy)
        getProp = getTextProperty d w wM_CLASS
        copy prop = fromMaybe "" . listToMaybe <$> wcTextPropertyToTextList d prop
    io $ getIt `E.catch` \(SomeException _) ->  (resName) `fmap` getClassHint d w
getWorkspace' :: Window -> X String
getWorkspace' w = withWindowSet $ \s -> do
                    case W.findTag w s >>= readWorkspaceInFamily of
                      Just wif -> return $ (familyId wif) ++ "|" ++ (workspaceId wif)
                      Nothing -> return ""

smartGreedyViewWindow = greedyViewWindow' True
greedyViewWindow = greedyViewWindow' False

greedyViewWindow' screenAware w  = do
  s <- gets windowset
  case W.findTag w s of
    Just tag -> do
      ifX screenAware $ do
        let fidMaybe = toFamilyIdMaybe tag
        caseMaybeJust (L.find ((fidMaybe ==) . toFamilyIdMaybe . W.tag . W.workspace) $ W.visible s) $ viewScreen . W.screen
      windows $ (W.focusWindow w) . (W.greedyView tag)
    Nothing -> windows $ W.focusWindow w

spawnAppSelectedTerminalAction' apps =
    runCyclicTerminalAction myTerminal "spawn.app" [mySpawnSelectedAppTerminalAction apps, dmenuRunTerminalAction]

myRunSelectedXTerminalAction = runSelectedXTerminalAction myTerminal selectActionTerminalActionTemplate
mySpawnSelectedAppTerminalAction = spawnSelectedAppTerminalAction selectActionTerminalActionTemplate
mySelectedXTerminalAction = selectedXTerminalAction selectActionTerminalActionTemplate

runDmenuRunTerminalAction = runTerminalAction myTerminal dmenuRunTerminalAction

runOpenDashboardTerminalAction = do
  runCyclicTerminalAction myTerminal "open.dashboard" $ L.map (\script -> openDashboardTerminalAction { actionScript = script }) [
                                  "~/.xmonad/terminal_actions/open_cluster_dashboard.sh",
                                  "~/.xmonad/terminal_actions/open_host_dashboard.sh",
                                  "~/.xmonad/terminal_actions/open_I.sh",
                                  "~/.xmonad/terminal_actions/open_I_K.sh"]
runOpenITerminalAction = do
  runCyclicTerminalAction myTerminal "open.I" $ L.map (\script -> openDashboardTerminalAction { actionScript = script }) [
                                  "~/.xmonad/terminal_actions/open_I.sh",
                                  "~/.xmonad/terminal_actions/open_I_K.sh"]
