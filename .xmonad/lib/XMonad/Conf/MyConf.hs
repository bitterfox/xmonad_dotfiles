module XMonad.Conf.MyConf where

import System.Directory
import System.Exit
import System.IO

import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W
import XMonad.ManageHook
import XMonad.Util.Run(runProcessWithInputAndWait, seconds)
import XMonad.Config.Gnome
import XMonad.Util.MyUtils

import XMonad.Actions.IntelliJTerminal
import XMonad.Util.NamedScratchpad2
import XMonad.Util.MyNamedScratchpad
import XMonad.Util.ManageHookUtils
import XMonad.Util.WorkspaceFamily

import XMonad.Actions.TerminalAction.GnomeBackend

import XMonad.Conf.MyParameters
import XMonad.Conf.MyTerminalAction

baseConfig = gnomeConfig

intelliJTerminalEnv =
  IntelliJTerminalEnvironment {
    homeDirectory = liftIO getHomeDirectory,
    XMonad.Actions.IntelliJTerminal.hook = onBottom
  }

------------------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------------------
systemActions = [
  ("Reload", myrestart),
  ("Lock", spawn "gnome-screensaver-command --lock"),
  ("Suspend", spawn "systemctl suspend"),
  ("Logout", io (exitWith ExitSuccess)),
  ("Shutdown", spawn "systemctl poweroff"),
  ("Reboot", spawn "systemctl reboot"),
  ("Toggle game mode", togglegamemode)
  ]

------------------------------------------------------------------------------------------
-- Scratchpad
------------------------------------------------------------------------------------------
myTerminalForScratchpad = myTerminal { extraArgs = "" }
myScratchpads :: [NamedScratchpad]
myScratchpads = [
    terminalScratchpad myTerminalForScratchpad "mainterm" Nothing $ onCenter' 0.01
  , terminalScratchpad myTerminalForScratchpad "term1" Nothing onTop
  , terminalScratchpad myTerminalForScratchpad "term2" Nothing onBottom
  , terminalScratchpad myTerminalForScratchpad "termL" Nothing onLeft
  , terminalScratchpad myTerminalForScratchpad "termR" Nothing onRight
  , terminalScratchpad myTerminalForScratchpad "jshell1" (Just jshellPath) onTop
  , terminalScratchpad myTerminalForScratchpad "jshell2" (Just jshellPath) onBottom
  , NS "bunnaru"
           "google-chrome --renderer-process-limit=1 --new-window --app=http://www.dmm.com/netgame/social/-/gadgets/=/app_id=798209/"
           (appName =? "www.dmm.com__netgame_social_-_gadgets_=_app_id=798209")
           (customFloating $ W.RationalRect 0 0.4 0.55 0.6)
  , NS "艦これ"
           "google-chrome --renderer-process-limit=1 --new-window --app=http://www.dmm.com/netgame/social/-/gadgets/=/app_id=854854/"
           (appName =? "www.dmm.com__netgame_social_-_gadgets_=_app_id=854854")
           (customFloating $ W.RationalRect 0.55 0.4 0.45 0.6)
  , NS "rhythmbox"
           "rhythmbox"
           (className =? "Rhythmbox")
           onCenter
  , terminalScratchpad myTerminalForScratchpad "fzf_actions" (Just "zsh -c '. $HOME/.fzf.zsh; _fzf_actions_then_echo -n | xsel -b -i'") $ onCenter'' 0.1 0.2
  , NS "ai-chatgpt"
            "bash ~/.xmonad/xmonad_ai.sh"
            (appName =? "chatgpt.com")
            $ onCenter''' 0.25 0.01 $ minWidth 1600
  , NS "ai-gemini"
            "bash ~/.xmonad/xmonad_ai.sh gemini"
            (appName =? "gemini.google.com")
            $ onCenter''' 0.25 0.01 $ minWidth 1600
 ]
myScratchpadsManageHook = namedScratchpadManageHook myScratchpads
myScratchpadsHandleEventHook =
    namedScratchpadHandleEventHook myScratchpads <+>
    (keepWindowSizeHandleEventHook $ intelliJTerminalQuery)
myNamedScratchpadAction = myNamedScratchpadActionInternal myScratchpads
myNamedScratchpadActionMaybe mns =
  whenJust mns $ \ns -> myNamedScratchpadAction $ name ns

myrestart = withWindowSet $ myrestart' . W.screen . W.current
myrestart' sid = do
  if sid == 0 then
      spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"
  else
      (viewScreen $ sid - 1) >> (myrestart' $ sid - 1)

togglegamemode = do
  homeDirectory <- liftIO getHomeDirectory
  runProcessWithInputAndWait "sh" ["-c", "bash '" ++ homeDirectory ++ "/.xmonad/toggle_game.sh' >> /tmp/xmonad.debug"] "" (seconds 1)
  myrestart

-- Workspace
originalWorkspaces = map show ([1 .. 9 :: Int] ++ [0])
workspaceFamilies = map show ([1 .. 9 :: Int] ++ [0])
myWorkspaces = expandWorkspacesToFamily workspaceFamilies originalWorkspaces
