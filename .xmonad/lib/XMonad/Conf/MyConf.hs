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
import XMonad.Layout.SimpleDecoration

import XMonad.Util.MyUtils

import XMonad.Actions.DrawShape (RGB(..))
import XMonad.Actions.IntelliJTerminal
import XMonad.Util.PhysicalScreen (EDID(..))
import XMonad.Util.NamedScratchpad2
import XMonad.Util.MyNamedScratchpad
import XMonad.Util.ManageHookUtils
import XMonad.Util.WorkspaceFamily

baseConfig = gnomeConfig

------------------------------------------------------------------------------------------
-- Color
------------------------------------------------------------------------------------------
black = "#4E4B42"
brightBlack = "#635F54"
gray = "#B4AF9A"
darkWhite = "#CDC8B0"
white = "#DAD4BB"
red = "#CC654C"
redRGB = RGB { rgb_red = 204, rgb_green = 101, rgb_blue = 76 }
blue = "#3BA99F"
blueRGB = RGB { rgb_red = 59, rgb_green = 169, rgb_blue = 159 }

-- #C14BCC
purpleRGB = RGB { rgb_red = 193, rgb_green = 75, rgb_blue = 204 }
-- #4B6BCC
darkBlueRGB = RGB { rgb_red = 75, rgb_green = 107, rgb_blue = 204 }
-- #A89F3B
yellowRGB = RGB { rgb_red = 168, rgb_green = 159, rgb_blue = 59 }

------------------------------------------------------------------------------------------
-- Application
------------------------------------------------------------------------------------------
intellijCommand = "~/bin/idea"
applications = [
-- ("Vivaldi (Web browser)", "export GDK_DPI_SCALE=1.02; vivaldi"),
 ("Vivaldi (Web browser)", "vivaldi --remote-debugging-port=9222"),
 ("Nautilus (File browser)", "nautilus"),
 ("Emacs (Editor)", "emacs"),
 -- ("LINE", "wine '/home/jp21734/.wine/drive_c/users/jp21734/Local Settings/Application Data/LINE/bin/LineLauncher.exe'"),
 ("LINE", "vivaldi-stable --app='chrome-extension://ophjlpahpchlmihnnnihgmmeilfjmjjc/index.html#/'"),
 ("Configuration", "XDG_CURRENT_DESKTOP=GNOME gnome-control-center"),
 ("LibreOffice", "libreoffice"),
 ("JetBrains ToolBox", "~/bin/jetbrains-toolbox-1.14.5179/jetbrains-toolbox"),
 ("IntelliJ Idea", intellijCommand),
 ("PulseSecure", "/opt/pulsesecure/bin/pulseUI"),
 ("Steam", "steam"),
 ("Slack", "slack"),
 ("Discord", "discord"),
 ("Tweetdeck", webApplication "https://tweetdeck.twitter.com/"),
 ("YouTube", webApplication "https://youtube.com/"),
 ("DAZN", webApplication "https://dazn.com/")]
webApplication url = "vivaldi-stable --app=" ++ url
javaHome = "~/.sdkman/candidates/java/current"
jshellPath = javaHome ++ "/bin/jshell"
intelliJTerminalEnv =
  IntelliJTerminalEnvironment {
    homeDirectory = liftIO getHomeDirectory,
    XMonad.Actions.IntelliJTerminal.hook = onBottom
  }

------------------------------------------------------------------------------------------
-- Display
------------------------------------------------------------------------------------------
priorityDisplayEDIDs :: [EDID]
priorityDisplayEDIDs = [
  "3840x2160+2294+0",
  "2294x1432+0+0",
  "1890x3360+6134+0",

  "00ffffffffffff0010ac4da2534e4a30", -- AW3225
  "00ffffffffffff0010ac49a2534e4a30",
  "00ffffffffffff0010acb7414c323332", -- U2720Q 9x16
  "00ffffffffffff0010acb4414c323332",
  "00ffffffffffff0010acb5414c323332",
  "00ffffffffffff00061044a000000000", -- Laptop display
  "00ffffffffffff0010acb3414c333232", -- U2720Q 16x9
  "00ffffffffffff0010acb5414c333232"]

------------------------------------------------------------------------------------------
-- Scratchpad
------------------------------------------------------------------------------------------
myScratchpads :: [NamedScratchpad]
myScratchpads = [
    terminalScratchpad "mainterm" Nothing $ onCenter' 0.01
  , terminalScratchpad "term1" Nothing onTop
  , terminalScratchpad "term2" Nothing onBottom
  , terminalScratchpad "termL" Nothing onLeft
  , terminalScratchpad "termR" Nothing onRight
  , terminalScratchpad "jshell1" (Just jshellPath) onTop
  , terminalScratchpad "jshell2" (Just jshellPath) onBottom
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
  , terminalScratchpad "fzf_actions" (Just "\"zsh -c '. $HOME/.fzf.zsh; _fzf_actions_then_echo -n | xsel -b -i'\"") $ onCenter'' 0.1 0.2
  , NS "ai"
            "bash ~/scripts/xmonad_ai.sh"
            --(className =? "xmonad.AI")
            (appName =? "chatgpt.com")
            $ onCenter''' 0.25 0.01 $ minWidth 1600
 ]
myScratchpadsManageHook = namedScratchpadManageHook myScratchpads
myScratchpadsHandleEventHook =
    namedScratchpadHandleEventHook myScratchpads <+>
    (keepWindowSizeHandleEventHook $ intelliJTerminalQuery)
myNamedScratchpadAction = myNamedScratchpadActionInternal myScratchpads
myNamedScratchpadActionMaybe mns =
  whenJust mns $ \ns -> myNamedScratchpadAction $ name ns

------------------------------------------------------------------------------------------
-- Action
------------------------------------------------------------------------------------------
systemActions = [
  ("Reload", myrestart),
  ("Lock", spawn "gnome-screensaver-command --lock"),
  ("Suspend", spawn "systemctl suspend"),
  ("Logout", io (exitWith ExitSuccess)),
  ("Shutdown", spawn "systemctl poweroff"),
  ("Reboot", spawn "systemctl reboot"),
  ("Toggle game mode", togglegamemode)
  ]

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

-- SD
mySDConfig = def {
               activeColor = white
             , inactiveColor = black
             , urgentColor = "white"
             , activeTextColor = black
             , inactiveTextColor = white
             , urgentTextColor = "red"
             , activeBorderColor = white
             , inactiveBorderColor = black
             , urgentBorderColor = "pink"
             , decoHeight = 32
             , fontName = "xft:monospace-9:bold,Symbola-9:bold"
}

-- Workspace
originalWorkspaces = map show ([1 .. 9 :: Int] ++ [0])
workspaceFamilies = map show ([1 .. 9 :: Int] ++ [0])
myWorkspaces = expandWorkspacesToFamily workspaceFamilies originalWorkspaces
